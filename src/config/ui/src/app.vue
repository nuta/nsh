<template>
<div id="app" class="container">
    <header>
        <h1>nshconfig</h1>
    </header>
    <main>
        <section>
            <h2>Startup Script</h2>
            <label for="rc">Startup Script (known as <code>.bashrc</code> in Bash)</label>
            <div class="input-group mb-3">
                <div id="rc" type="text" class="editor"></div>
            </div>
        </section>

        <section>
            <h2>Prompt</h2>
            <label for="prompt">Prompt Format (<code>$prompt</code>)</label>
            <div class="input-group mb-3">
              <div class="input-group-prepend">
                <span class="input-group-text">$prompt =</span>
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
                            <th scope="row"><code>\if{cond}{then}{else}</code></th>
                            <td colspan="5">A ternary expression, e.g.
                                <code>\if{in_git_repo}{at \{git_branch}}{}</code><br>
                                <b>(preview is not yet implemented)</b></td>
                        </tr>
                    </tbody>
                </table>
            </div>

            <h6><code>\if</code> conditions:</h6>
            <div class="alert alert-info" role="alert">
                <table class="table table-borderless table-sm">
                    <tbody>
                        <tr>
                            <th scope="row"><code>in_git_repo</code></th>
                            <td>True if the current working directory is in a Git repository.</td>
                        </tr>
                    </tbody>
                </table>
            </div>
        </section>

        <section>
            <h2>PATH</h2>
            <label for="path">The executable directories separated by <code>:</code> (colon).</label>
            <div class="input-group mb-3">
                <textarea id="path" type="text" class="form-control" v-model="path"></textarea>
            </div>
        </section>
    </main>
</div>
</template>

<script lang="ts">
import Noty from 'noty';
import request from 'superagent';
import { setTimeout, clearTimeout } from 'timers';

const SETTINGS = [
    {
        name: "rc",
        type: "string",
        default: ""
    },
    {
        name: "prompt",
        type: "string",
        default: "\\{cyan}\\{bold}[\\{username}@\\{hostname}]:\\{reset} \\{current_dir} $\\{reset} "
    },
    {
        name: "path",
        type: "string",
        default: "/bin:/usr/bin:/usr/local/bin:/sbin"
    }
];


function parse_prompt(prompt: string): any[] {
    let frags = [];
    let color = "white";
    let bold = false;
    let underline = false;
    let skip_next = false;
    let rest = prompt;
    while (rest.length > 0) {
        let text = null;
        if (rest[0]== "\\") {
            // Consume the backslash to prevent an infinite loop by
            // an unsupported escape sequence.
            rest = rest.substring(1);

            if (rest.startsWith("{username}")) {
                text = "username";
                rest = rest.substring(10);
            }

            if (rest.startsWith("{hostname}")) {
                text = "hostname.local";
                rest = rest.substring(10);
            }

            if (rest.startsWith("{current_dir}")) {
                text = "~/Documents";
                rest = rest.substring(13);
            }

            for (const color_name of ["red", "blue", "green", "yellow", "cyan", "magenta"]) {
                if (rest.startsWith("{" + color_name + "}")) {
                    text = "";
                    color = color_name;
                    rest = rest.substring(2 + color_name.length);
                    break;
                }
            }

            if (rest.startsWith("{bold}")) {
                text = "";
                bold = true;
                rest = rest.substring(6);
            }

            if (rest.startsWith("{underline}")) {
                text = "";
                underline = true;
                rest = rest.substring(11);
            }

            if (rest.startsWith("{reset}")) {
                text = "";
                bold = false;
                underline = false;
                color = "white";
                rest = rest.substring(7);
            }

            // TODO: support \if
        }

        if (text == null) {
            let next_span_index = rest.indexOf("\\");
            if (next_span_index == -1) {
                text = rest;
                rest = "";
            } else {
                text = rest.substring(0, next_span_index);
                rest = rest.substring(next_span_index);
            }

        }

        frags.push({ color, bold, underline, text }) ;
    }

    return frags;
}

export default {
    data () {
        const params = new URLSearchParams(window.location.search);
        let data = {
            access_token: params.get("access_token"),
        };

        for (const s of SETTINGS) {
            data[s.name] = s.default;
        }
        return data;
    },
    computed: {
        prompt_preview() {
            return parse_prompt(this.prompt);
        }
    },
    async beforeMount() {
        const resp = await request
          .get("/api/load")
          .query({ access_token: this.access_token });

        for (const s of SETTINGS) {
            this[s.name] = resp.body[s.name] || s.default;
        }

        // Initialize Ace editor.
        var editor = ace.edit("rc");
        editor.setTheme("ace/theme/monokai");
        editor.setShowPrintMargin(false);
        editor.session.setMode("ace/mode/sh");
        editor.session.setValue(this.rc);
        editor.session.on('change', () => {
            this.rc = editor.session.getValue();
        });

        // Autosave
        let timer = null;
        this.$watch(
            () => {
                let data = {};
                for (const s of SETTINGS) {
                    data[s.name] = this.$data[s.name];
                }
                return data;
            },
            (new_value) => {
                if (timer) {
                    clearTimeout(timer);
                }

                timer = setTimeout(() => {
                    request
                        .post("/api/save")
                        .query({ access_token: this.access_token })
                        .send(new_value)
                        .then(() => {
                            new Noty({
                                text: '<b>saved to ~/.nshconfig</b>',
                                type: 'success',
                                timeout: 1500,
                             }).show();
                        })
                        .catch(err => {
                            console.error(err);
                            new Noty({
                                text: "something went wrong (check out developer tools)",
                                type: 'error',
                             }).show();
                        });
                }, 1000);
            }
        );
    },
};
</script>

<style lang="scss">
@import "~noty/src/noty.scss";
@import "~noty/src/themes/mint.scss";

h1 {
    margin: 15px 0px;
    text-align: center;
}

body {
    background: #74bed8;
}

main {
    margin: 0px auto 30px;
    padding: 40px 30px;
    width: 900px;
    border-radius: 5px;
    background: #fefefe;
}

section {
    &:not(:nth-child(1)) {
        border-top: 3px solid #cacaca;
        padding-top: 35px;
        margin-top: 35px;
    }
}

input, textarea {
    font-family: "Source Code Pro", "Monaco", monospace;
}

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

.editor {
    height: 400px;
    width: 100%;
}
</style>
