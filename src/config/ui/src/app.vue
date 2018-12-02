<template>
<div id="app" class="container">
    <header>
        <h1>nshconfig</h1>
    </header>
    <main>
        <section>
            <h2>Prompt</h2>
            <label for="ps1">Prompt Format (<code>$PS1</code>)</label>
            <div class="input-group mb-3">
              <div class="input-group-prepend">
                <span class="input-group-text">$PS1 =</span>
              </div>
              <input type="text" class="form-control" id="ps1" v-model="ps1">
            </div>
            <p>Preview:</p>
            <div class="ps1-preview">
                <span v-for="frag in ps1_preview"
                :class="['frag', 'color-' + frag.color, { bold: frag.bold, underline: frag.underline }]"
                ><template v-if="frag.text == '\n'"><br></template><template v-else>{{ frag.text }}</template></span>
            </div>
            <div class="alert alert-info" role="alert">
                <table class="table table-borderless table-sm">
                    <tbody>
                        <tr>
                            <th scope="row"><code>\u</code></th>
                            <td>User name</td>
                            <th scope="row"><code>\h</code></th>
                            <td>Host name</td>
                            <th scope="row"><code>\W</code></th>
                            <td>Current directory</td>
                        </tr>
                        <tr>
                            <th scope="row"><code>\n</code></th>
                            <td>Newline</td>
                            <th scope="row"><code>\c{bold}</code></th>
                            <td>Bold (Color)</td>
                            <th scope="row"><code>\c{underline}</code></th>
                            <td>Underline</td>
                        </tr>
                        <tr>
                            <th scope="row"><code>\c{red}</code></th>
                            <td>Red (Color)</td>
                            <th scope="row"><code>\c{blue}</code></th>
                            <td>Blue (Color)</td>
                            <th scope="row"><code>\c{green}</code></th>
                            <td>Green (Color)</td>
                        </tr>
                        <tr>
                            <th scope="row"><code>\c{yellow}</code></th>
                            <td>Yellow (Color)</td>
                            <th scope="row"><code>\c{cyan}</code></th>
                            <td>Cyan (Color)</td>
                            <th scope="row"><code>\c{magenta}</code></th>
                            <td>Magenta (Color)</td>
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
        name: "ps1",
        type: "string",
        default: "\\c{cyan}\\c{bold}[\\u@\\h]:\\c{reset} \\W $\\c{reset} "
    },
    {
        name: "path",
        type: "string",
        default: "/bin:/usr/bin:/usr/local/bin:/sbin"
    }
];


function parse_ps1(ps1: string): any[] {
    let frags = [];
    let color = "white";
    let bold = false;
    let underline = false;
    let skip_next = false;
    for (let i = 0; i < ps1.length; i++) {
        const crnt = ps1[i];
        const next = ps1[i + 1] || '';
        if (skip_next) {
            skip_next = false;
            continue;
        }

        switch (crnt) {
            case '\\':
                skip_next = true;
                let text = '';
                switch (next) {
                    case 'u': text = 'username'; break;
                    case 'h': text = 'hostname.local'; break;
                    case 'W': text = '~/Development'; break;
                    case 'n': text = "\n"; break;
                    case 'c':
                        let attr = "";
                        // Skip `\\', `c', and `{'.
                        i += 3;
                        for (; i < ps1.length; i++) {
                            if (ps1[i] == '}') {
                                break;
                            }

                            attr += ps1[i];
                        }

                        switch (attr) {
                            case "reset":
                                color = "white";
                                bold = false;
                                break;
                            case "bold":
                                bold = true;
                                break;
                            case "underline":
                                underline = true;
                                break;
                            case "red":
                            case "blue":
                            case "green":
                            case "yellow":
                            case "cyan":
                            case "magenta":
                                color = attr;
                                break;
                        }
                    default:
                        text = '';
                        skip_next = false;
                }
                frags.push({ color, bold, underline, text });
                break;
            default:
                frags.push({ color, bold, underline, text: crnt });
        }
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
        ps1_preview() {
            return parse_ps1(this.ps1);
        }
    },
    watch: {

    },
    methods: {
    },
    async beforeMount() {
        const resp = await request
          .get("/api/load")
          .query({ access_token: this.access_token });

        for (const s of SETTINGS) {
            this[s.name] = resp.body[s.name] || s.default;
        }

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
                                text: '<b>saved to ~/.nshinit</b>',
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
    }
};
</script>

<style lang="scss">
@import "~noty/src/noty.scss";
@import "~noty/src/themes/mint.scss";

section {
    border-top: 3px solid #cacaca;
    padding-top: 35px;
    margin-top: 35px;
}

.ps1-preview {
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
