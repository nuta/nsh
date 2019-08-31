<template>
<div>
    <h2>Prompt</h2>
    <label for="prompt">Prompt Format (<code>$PROMPT</code> a.k.a. <code>$PS1</code>)</label>
    <div class="input-group mb-3">
      <div class="input-group-prepend">
        <span class="input-group-text">$PROMPT =</span>
      </div>
      <input type="text" class="form-control" id="prompt" v-model="prompt">
    </div>

    <p>Preview:</p>
    <div class="prompt-preview">
        <span v-for="frag in prompt_preview"
        :class="['frag', 'color-' + frag.color, { bold: frag.bold, underline: frag.underline }]"
        ><template v-if="frag.text == '\n'"><br></template><template v-else>{{ frag.text }}</template></span>
    </div>

    <div class="alert alert-info" role="alert">
        <table class="table table-borderless table-sm">
            <tbody>
                <tr>
                    <th scope="row"><code>\{username}</code></th>
                    <td>User name</td>
                    <th scope="row"><code>\{hostname}</code></th>
                    <td>Host name</td>
                    <th scope="row"><code>\{current_dir}</code></th>
                    <td>Current directory</td>
                </tr>
                <tr>
                    <th scope="row"><code>\n</code></th>
                    <td>Newline</td>
                    <th scope="row"><code>\{bold}</code></th>
                    <td>Bold (Color)</td>
                    <th scope="row"><code>\{underline}</code></th>
                    <td>Underline</td>
                </tr>
                <tr>
                    <th scope="row"><code>\{red}</code></th>
                    <td>Red (Color)</td>
                    <th scope="row"><code>\{blue}</code></th>
                    <td>Blue (Color)</td>
                    <th scope="row"><code>\{green}</code></th>
                    <td>Green (Color)</td>
                </tr>
                <tr>
                    <th scope="row"><code>\{yellow}</code></th>
                    <td>Yellow (Color)</td>
                    <th scope="row"><code>\{cyan}</code></th>
                    <td>Cyan (Color)</td>
                    <th scope="row"><code>\{magenta}</code></th>
                    <td>Magenta (Color)</td>
                </tr>
                <tr>
                    <th scope="row"><code>\{repo_status}</code></th>
                    <td>current repository status (Git only)</td>
                    <th scope="row"><code></code></th>
                    <td></td>
                    <th scope="row"><code></code></th>
                    <td></td>
                </tr>
                <tr>
                    <th scope="row"><code>\if{cond}{then}{else}</code></th>
                    <td colspan="5">A ternary expression, e.g.
                        <code>\if{in_repo}{at \{repo_status}}{}</code><br>
                        <b></b></td>
                </tr>
            </tbody>
        </table>
    </div>

    <h6><code>\if</code> conditions:</h6>
    <div class="alert alert-info" role="alert">
        <table class="table table-borderless table-sm">
            <thead>
                <tr>
                    <th scope="col">Name</th>
                    <th scope="col">Description</th>
                    <th scope="col">Preview</th>
                </tr>
            </thead>
            <tbody>
                <tr>
                    <th scope="row"><code>in_repo</code></th>
                    <td>True if the current working directory is in a repository.</td>
                    <td>
                        <select class="form-control" v-model="conditions.in_repo">
                            <option value="true">true</option>
                            <option value="false">false</option>
                        </select>
                    </td>
                </tr>
            </tbody>
        </table>
    </div>
</div>
</template>

<script lang="ts">

function extract_block(prompt: string): string {
    if (!prompt.startsWith("{")) {
        return prompt;
    }

    let part = "";
    let level = 1;
    let i;
    for (i = 1; i < prompt.length; i++) {
        if (prompt[i] == "{") {
            level++;
        }

        if (prompt[i] == "}") {
            level--;
            if (level == 0) {
                return part;
            }
        }

        part += prompt[i];
    }
}

function parse_prompt(prompt: string, conditions: {[name: string]: string }): any[] {
    let frags = [];
    let color = "white";
    let bold = false;
    let underline = false;
    let skip_next = false;

    const placeholders = {
        username: "username",
        hostname: "hostname.local",
        current_dir: "~/Documents",
        repo_status: "master",
    }

    const simple_span = /^\\\{(username|hostname|current_dir|repo_status)\}/;
    const color_span = /^\\\{(red|blue|green|yellow|cyan|magenta)\}/;
    const bold_span = /^\\\{bold\}/;
    const underline_span = /^\\\{underline\}/;
    const reset_span = /^\\\{reset\}/;
    const newline_span = /^\\n/;
    const if_span = /^\\if{([^}]+)}/;
    while (prompt.length > 0) {
        let m, text;
        if ((m = prompt.match(simple_span)) !== null) {
            text = placeholders[m[1]];
        } else if ((m = prompt.match(color_span)) !== null) {
            color = m[1];
        } else if ((m = prompt.match(bold_span)) !== null) {
            bold = true;
        } else if ((m = prompt.match(underline_span)) !== null) {
            underline = true;
        } else if ((m = prompt.match(reset_span)) !== null) {
            color = "white";
            bold = false;
            underline = false;
        } else if ((m = prompt.match(newline_span)) !== null) {
            text = "\n";
        } else if ((m = prompt.match(if_span)) !== null) {
            prompt = prompt.substring(m[0].length);
            const cond = m[1];
            const then_part = extract_block(prompt);
            prompt = prompt.substring(then_part.length + 2 /* skip `{' and `}' */);
            const else_part = extract_block(prompt);
            prompt = prompt.substring(else_part.length + 2 /* skip `{' and `}' */);

            if (conditions[cond] === "true") {
                frags.push(...parse_prompt(then_part, conditions));
            } else {
                frags.push(...parse_prompt(else_part, conditions));
            }

            continue;
        } else {
            // A normal character.
            frags.push({ color, bold, underline, text: prompt[0] });
            prompt = prompt.substring(1);
            continue;
        }

        // Matched to a special span.
        const entire = m[0];
        const matched = m[1];
        frags.push({ color, bold, underline, text });
        prompt = prompt.substring(entire.length);
    }

    return frags;
}

export default {
    props: ["prompt"],
    data() {
        return {
            conditions: {
                in_repo: "true",
            }
        }
    },
    watch: {
        prompt() {
            // FIXME:
            this.$emit('update:prompt', this.prompt);
        }
    },
    computed: {
        prompt_preview() {
            return parse_prompt(this.prompt, this.conditions);
        }
    },

}
</script>

<style lang="scss" scoped>

.prompt-preview {
    width: 100%;
    padding: 20px 30px;
    margin-bottom: 30px;
    font-size: 20px;
    font-family: "Source Code Pro", "Monaco", monospace;
    color: #fefefe;
    background: #303030;

    .bold          { font-weight: bold; }
    .underline     { text-decoration: underline; }
    .color-red     { color: red; }
    .color-blue    { color: blue; }
    .color-green   { color: green; }
    .color-yellow  { color: yellow; }
    .color-cyan    { color: cyan; }
    .color-magenta { color: magenta; }
}

</style>
