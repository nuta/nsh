use crate::context_parser::{self, InputContext};
use crate::eval::*;
use crate::fuzzy::FuzzyVec;
use crate::process::*;
use crate::shell::Shell;
use crate::variable::*;
use dirs;
use std::collections::VecDeque;
use std::os::unix::fs::PermissionsExt;
use std::path::{Path, PathBuf};

/// A completion-related prompt states.
pub struct CompletionSelector {
    /// Candidate completion entries.
    entries: Vec<String>,
    // The currently selected entry.
    selected_index: usize,
    // The number of completion lines in the prompt.
    display_lines: usize,
    // The beginning of entries to be displayed.
    display_index: usize,
}

impl CompletionSelector {
    pub fn new(entries: Vec<String>) -> CompletionSelector {
        const COMPLETION_LINES: usize = 7;

        CompletionSelector {
            entries,
            selected_index: 0,
            display_lines: COMPLETION_LINES,
            display_index: 0,
        }
    }

    /// Move to the next/previous entry.
    pub fn move_cursor(&mut self, offset: isize) {
        // FIXME: I think there's more sane way to handle a overflow.`
        let mut old_selected_index = self.selected_index as isize;
        old_selected_index += offset;

        let entries_len = self.len() as isize;
        if entries_len > 0 && old_selected_index > entries_len - 1 {
            old_selected_index = entries_len - 1;
        }

        if old_selected_index < 0 {
            old_selected_index = 0;
        }

        self.selected_index = old_selected_index as usize;

        if self.selected_index >= self.display_index + self.display_lines {
            self.display_index = self.selected_index - self.display_lines + 1;
        }

        if self.selected_index < self.display_index {
            self.display_index = self.selected_index;
        }

        trace!(
            "completion_cursor: move_offset={}, display={}, selected={}",
            offset,
            self.display_index,
            self.selected_index
        );
    }

    #[inline(always)]
    pub fn entries(&self) -> Vec<String> {
        self.entries.clone()
    }

    #[inline(always)]
    pub fn len(&self) -> usize {
        self.entries.len()
    }

    #[inline(always)]
    pub fn selected_index(&self) -> usize {
        self.selected_index
    }

    #[inline(always)]
    pub fn get(&self, index: usize) -> Option<&str> {
        self.entries.get(index).map(String::as_str)
    }

    #[inline(always)]
    pub fn display_lines(&self) -> usize {
        self.display_lines
    }

    #[inline(always)]
    pub fn display_index(&self) -> usize {
        self.display_index
    }

    pub fn select_and_update_input_and_cursor(
        &self,
        ctx: &InputContext,
        user_input: &str,
        user_cursor: &mut usize,
    ) -> String {
        let selected = match self.get(self.selected_index()) {
            Some(selected) => selected,
            None => return user_input.to_owned(),
        };

        let (prefix_end, suffix_start) = match &ctx.current_literal {
            Some(current_literal) => (current_literal.start, current_literal.end + 1),
            None => (ctx.cursor, ctx.cursor + 1),
        };

        let prefix: String = user_input.chars().take(prefix_end).collect();
        let suffix: String = user_input.chars().skip(suffix_start).collect();

        let path = if selected.starts_with("~/") {
            let mut path = dirs::home_dir().unwrap().to_path_buf();
            let mut sub_path = PathBuf::from(selected);
            sub_path = sub_path.strip_prefix("~/").unwrap().to_path_buf();
            path.push(sub_path);
            path
        } else {
            PathBuf::from(selected)
        };

        // add a slash or space after the word.
        let append = if path.is_dir() { "/" } else { " " };

        trace!(
            "complete: '{}' '{}' '{}' '{}'",
            prefix,
            selected,
            append,
            suffix
        );
        *user_cursor = prefix.chars().count() + selected.chars().count() + append.chars().count();
        return format!("{}{}{}{}", prefix, selected, append, suffix);
    }
}

/// `compgen(1)`.
pub struct CompGen {
    entries: Vec<String>,
    query: Option<String>,
    /// `-A command`
    include_commands: bool,
    /// `-A file`
    include_files: bool,
    /// `-A directory`
    include_dirs: bool,
}

impl CompGen {
    #[inline]
    pub fn new() -> CompGen {
        CompGen {
            entries: Vec::new(),
            query: None,
            include_commands: false,
            include_files: false,
            include_dirs: false,
        }
    }

    /// -A command / -c
    #[inline]
    pub fn include_commands(&mut self, enable: bool) -> &mut CompGen {
        self.include_commands = enable;
        self
    }

    /// -A file / -f
    #[inline]
    pub fn include_files(&mut self, enable: bool) -> &mut CompGen {
        self.include_files = enable;
        self
    }

    /// -A directory / -d
    #[inline]
    pub fn include_dirs(&mut self, enable: bool) -> &mut CompGen {
        self.include_dirs = enable;
        self
    }

    /// -W
    #[inline]
    pub fn wordlist<'a>(&'a mut self, wordlist: &str, ifs: &str) -> &'a mut CompGen {
        self.entries = wordlist
            .split(|c| ifs.contains(c))
            .map(|elem| elem.to_owned())
            .collect();

        self
    }

    #[inline]
    pub fn entries(&mut self, entries: Vec<String>) -> &mut CompGen {
        self.entries = entries;
        self
    }

    #[inline]
    pub fn filter_by(&mut self, query: &str) -> &mut CompGen {
        self.query = Some(query.to_owned());
        self
    }

    #[inline]
    pub fn generate(self) -> Vec<String> {
        match self.query {
            Some(query) => FuzzyVec::from_vec(self.entries).search(&query),
            None => self.entries,
        }
    }
}

/// Returns true if the directory should not be scanned.
fn dir_scan_filter(path: &Path) -> bool {
    // .git
    path.iter().any(|s| s == ".git")
}

/// Returns file paths. It scans *recursively* from the given (or current) directory.
pub fn path_completion(
    ctx: &InputContext,
    include_files: bool,
    include_dirs: bool,
    executable_only: bool,
    remove_dot_slash_prefix: bool,
) -> Vec<String> {
    let current_word = match &ctx.current_literal {
        Some(range) => Some(ctx.input.as_str()[range.clone()].to_owned()),
        None => None,
    };
    let mut remaining_dirs = VecDeque::new();
    let given_dir = current_word.clone();
    let home_dir = dirs::home_dir().unwrap();

    trace!("path_completion: word='{:?}'", current_word);
    match &given_dir {
        // ~/Downloads/monica-lottery
        Some(given_dir) if given_dir.starts_with("~/") => {
            let mut path = PathBuf::from(&home_dir);
            let mut sub_path = PathBuf::from(&given_dir);

            // Remove `~/': `~/Downloads/monica-lottery' -> `Downloads/monica-lottery'
            sub_path = sub_path.strip_prefix("~/").unwrap().to_path_buf();
            if !given_dir.ends_with('/') {
                // `~/Downloads/monica-lottery' -> `~/Downloads'
                sub_path = sub_path.parent().unwrap_or(&sub_path).to_path_buf();
            }

            path.push(sub_path);
            remaining_dirs.push_front(path);
        }
        // Downloads/
        Some(given_dir) if given_dir.ends_with('/') => {
            remaining_dirs.push_front(PathBuf::from(given_dir));
        }
        // Downloads/monica-lotte
        Some(given_dir) if given_dir.contains('/') => {
            // Remove the last part: `Downloads/monica-lotte' -> `Downloads'
            remaining_dirs.push_front(
                PathBuf::from(given_dir.clone())
                    .parent()
                    .unwrap()
                    .to_path_buf(),
            );
        }
        // Download
        _ => {
            remaining_dirs.push_front(PathBuf::from("."));
        }
    }

    let mut entries = Vec::new();
    let mut max_scan = 1024; // TODO: compute this by machine performance

    // Scan the directories in breadth-first way.
    'scan_loop: while let Some(dir_path) = remaining_dirs.pop_front() {
        if let Ok(dirent) = std::fs::read_dir(dir_path) {
            for entry in dirent {
                if max_scan < 0 {
                    // Too many files. Abort scanning.
                    break 'scan_loop;
                }
                max_scan -= 1;

                let entry = entry.unwrap();
                let file_type = entry.file_type().unwrap();

                if file_type.is_dir() && !dir_scan_filter(&entry.path()) {
                    // The entry is a directory. Append to the queue to scan it
                    // later.
                    remaining_dirs.push_back(entry.path());
                }

                let perm = entry.metadata().unwrap().permissions();
                if executable_only && (file_type.is_file() && perm.mode() & 0b001_001_001 == 0) {
                    // Executable_only is enabled and the file `entry' is not
                    // a execuatble. Ignore it.
                    continue;
                }

                if (include_files && file_type.is_file()) || (include_dirs && file_type.is_dir()) {
                    // The entry should be added to the result.
                    let mut path = entry.path();

                    if path.starts_with(&home_dir) {
                        // The path is in the home directory. Replace the prefix
                        // with `~/'.
                        path = PathBuf::from("~")
                            .join(path.strip_prefix(&home_dir).unwrap())
                            .to_path_buf();
                    }

                    if remove_dot_slash_prefix && path.starts_with("./") {
                        // `./Downloads/monica-lottery.txt` -> `Downloads/monica-lottery.txt`
                        path = path.strip_prefix("./").unwrap().to_path_buf();
                    }

                    entries.push(path.to_str().unwrap().to_owned());
                }
            }
        }
    }

    // Filter the results by the current word at the user cursor.
    let mut compgen = CompGen::new();
    compgen.entries(entries);
    if let Some(current_word) = current_word {
        compgen.filter_by(current_word.trim_start_matches("~/"));
    }

    compgen.generate()
}

pub fn cmd_completion(shell: &Shell, ctx: &InputContext) -> Vec<String> {
    match &ctx.current_literal {
        Some(range) => {
            let query = &ctx.input[range.clone()];
            let mut entries = shell.path_table.complete(query);
            entries.extend(path_completion(ctx, true, false, true, false));
            entries
        }
        None => shell.path_table.complete(""),
    }
}

/// A completion definition.
#[derive(Debug, Clone)]
pub struct CompSpec {
    /// `-F func_name`
    func_name: Option<String>,
    /// `-o filenames`
    filenames_if_empty: bool,
    /// `-o dirnames`
    dirnames_if_empty: bool,
}

impl CompSpec {
    #[inline]
    pub fn func_name(&self) -> Option<&String> {
        self.func_name.as_ref()
    }

    #[inline]
    pub fn filenames_if_empty(&self) -> bool {
        self.filenames_if_empty
    }

    #[inline]
    pub fn dirnames_if_empty(&self) -> bool {
        self.dirnames_if_empty
    }
}

#[derive(Debug)]
pub struct CompSpecBuilder {
    func_name: Option<String>,
    filenames_if_empty: bool,
    dirnames_if_empty: bool,
}

impl CompSpecBuilder {
    pub fn new() -> CompSpecBuilder {
        CompSpecBuilder {
            func_name: None,
            filenames_if_empty: false,
            dirnames_if_empty: false,
        }
    }

    /// -F func_name
    #[inline]
    pub fn func_name(&mut self, func_name: String) -> &mut CompSpecBuilder {
        self.func_name = Some(func_name);
        self
    }

    /// -o dirnames
    #[inline]
    pub fn dirnames_if_empty(&mut self, enable: bool) -> &mut CompSpecBuilder {
        self.dirnames_if_empty = enable;
        self
    }

    /// -o filenames
    #[inline]
    pub fn filenames_if_empty(&mut self, enable: bool) -> &mut CompSpecBuilder {
        self.filenames_if_empty = enable;
        self
    }

    #[inline]
    pub fn build(self) -> CompSpec {
        CompSpec {
            func_name: self.func_name,
            filenames_if_empty: self.filenames_if_empty,
            dirnames_if_empty: self.dirnames_if_empty,
        }
    }
}

fn call_completion_function(shell: &mut Shell, func_name: &str, ctx: &InputContext) -> Vec<String> {
    let locals = vec![
        ("COMP_WORDS", Value::Array(ctx.words.clone())),
        ("COMP_CWORD", Value::String(ctx.current_word.to_string())),
    ];

    match call_function_in_shell_context(shell, func_name, &[], locals) {
        Ok(ExitStatus::ExitedWith(0)) => shell
            .get("COMPREPLY")
            .and_then(|reply| match reply.value() {
                Some(Value::Array(arr)) => Some(arr.clone()),
                _ => None,
            })
            .unwrap_or_else(Vec::new),
        Ok(status) => {
            // Something went wrong within the function. Just ignore it.
            eprintln!(
                "nsh: failed to invoke a completion function `{}': {:?}",
                func_name, status
            );
            Vec::new()
        }
        Err(err) => {
            // Something went wrong (BUG).
            eprintln!(
                "nsh: BUG: failed to invoke a function `{}': {}",
                func_name, err
            );
            Vec::new()
        }
    }
}

/// Returns completion candidates.
pub fn complete(shell: &mut Shell, ctx: &InputContext) -> Vec<String> {
    trace!("complete: ctx={:?}", ctx);

    let cmd_name = if let Some(name) = ctx.words.get(0) {
        let name = name.as_str().to_owned();
        match shell.lookup_alias(&name) {
            Some(alias) if !alias.contains(' ') => {
                // The alias named `name' is defined and it is a command
                // (does not contain whitespaces). Use its value as the
                // target command name.
                alias.to_owned()
            }
            _ => name,
        }
    } else {
        "".to_owned()
    };

    let current_span = ctx.current_span.map(|index| &ctx.spans[index]);
    if let Some(context_parser::Span::Argv0(_)) = current_span {
        // The cursor is at the first word, namely, the command.
        cmd_completion(shell, ctx)
    } else if let Some(compspec) = shell.get_compspec(cmd_name.as_str()) {
        let compspec = compspec.clone();
        let mut results = Vec::new();
        if let Some(name) = compspec.func_name() {
            results = call_completion_function(shell, &name, ctx);
        }

        if results.is_empty() {
            // No completion candidates. Try defaults.
            results.extend(path_completion(
                ctx,
                compspec.filenames_if_empty(),
                compspec.dirnames_if_empty(),
                false,
                true,
            ));
        }

        results
    } else {
        // Compspec if not defined for the command. Fall back to the path
        // completion.
        path_completion(ctx, true, true, false, true)
    }
}
