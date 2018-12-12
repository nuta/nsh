<template>
<div id="app" class="container">
    <header>
        <h1>nshconfig editor</h1>
    </header>
    <main>
        <ul class="nav nav-tabs" role="tablist">
            <li class="nav-item">
                <a class="nav-link active" id="rc-tab" data-toggle="tab" href="#rc" role="tab" aria-controls="rc" aria-selected="true">Startup Script</a>
            </li>
            <li class="nav-item">
                <a class="nav-link" id="prompt-tab" data-toggle="tab" href="#prompt" role="tab" aria-controls="prompt" aria-selected="false">Prompt</a>
            </li>
            <li class="nav-item">
                <a class="nav-link" id="env-tab" data-toggle="tab" href="#env" role="tab" aria-controls="env" aria-selected="false">Environment Variables</a>
            </li>
        </ul>
        <div class="tab-content">
            <div class="tab-pane fade show active" id="rc" role="tabpanel" aria-labelledby="rc-tab">
                <rc-settings :script="rc" @changed="updateRc"></rc-settings>
            </div>
            <div class="tab-pane fade show" id="prompt" role="tabpanel" aria-labelledby="prompt-tab">
                <prompt-settings :prompt.sync="prompt"></prompt-settings>
            </div>
            <div class="tab-pane fade show" id="env" role="tabpanel" aria-labelledby="env-tab">
                <env-settings :path="path"></env-settings>
            </div>
        </div>
    </main>
</div>
</template>

<script lang="ts">
import Noty from 'noty';
import request from 'superagent';
import PromptSettings from './components/prompt.vue';
import RcSettings from './components/rc.vue';
import EnvSettings from './components/env.vue';
import { setTimeout, clearTimeout } from 'timers';

const SETTINGS = [
    {
        name: "rc",
        type: "string",
        default: "# Write your startup shell script!"
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
    },
];


export default {
    components: { RcSettings, PromptSettings, EnvSettings },
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
    methods: {
        updateRc(script) {
            this.rc = script;
        },
    },
    async beforeMount() {
        const resp = await request
          .get("/api/load")
          .query({ access_token: this.access_token });

        const body = resp.body || {};
        let num_undefined = 0;
        for (const s of SETTINGS) {
            if (body[s.name] === undefined && num_undefined < 5) {
                console.error(`\`${s.name}' is not defined in ~/.nshconfig (using default value instead).`);
                num_undefined++;
            }

            this[s.name] = body[s.name] || s.default;
        }

        if (num_undefined) {
                new Noty({
                    text: "<b>Some settings are not defined in .nshconfig<br>(using default values instead)</b>",
                    type: "warning",
                }).show();
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
                        .set("Content-Type", "application/octet-stream")
                        .query({ access_token: this.access_token })
                        .send(JSON.stringify(new_value, null, 4))
                        .then(() => {
                            new Noty({
                                text: "<b>saved to ~/.nshconfig</b>",
                                type: "success",
                                timeout: 1500,
                             }).show();
                        })
                        .catch(err => {
                            console.error(err);
                            new Noty({
                                text: "<b>something went wrong (check out developer tools)</b>",
                                type: "error",
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
    color: #2a2a2a;
    margin-top: 35px;
    margin-bottom: 20px;
    text-align: center;
    font-family: "Source Code Pro", "Monaco", monospace;
    font-size: 28px;
}

h2 {
    margin: 15px 0px;
}

body {
    background: #74bed8;
}

main {
    margin: 0px auto 30px;
    padding: 40px 30px;
    width: 1000px;
    border-radius: 5px;
    background: #fefefe;
}

input, textarea {
    font-family: "Source Code Pro", "Monaco", monospace;
}
</style>
