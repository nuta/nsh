<template>
<div>
    <h2>Migrate zsh history (<code>EXTENDED_HISTORY</code> format) into nsh</h2>
    <div class="alert alert-warning" role="alert">
        <code>~/.nsh_history</code> will be overwritten.
    </div>
    <div class="form-group">
        <input type="file" id="file">
    </div>
    <div class="form-group">
        <button @click.prevent="save" class="btn btn-danger">
            Convert and save to ~/.nsh_history
        </button>
    </div>
</div>
</template>

<script lang="ts">
import Noty from 'noty';
import Vue from 'vue';
import request from 'superagent';

function convert(zsh_hist: string): string {
    let nsh_hist = "";
    let ignored = 0;
    let converted = 0;
    for (const line of zsh_hist.split("\n")) {
        if (!line.startsWith(": ")) {
            // Not a valid format.
            ignored += 1;
            continue;
        }

        const [context, cmd] = line.split(";", 2);
        const [_prefix, time, _repeated] = context.split(":");

        const entry = {
            time: parseInt(time.trim()),
            cwd: "",
            cmd
        };
        nsh_hist += JSON.stringify(entry) + "\n";
        converted++;
    }

    console.info(`converted ${converted} entries and ignored ${ignored} entries`);
    return nsh_hist;
}

export default Vue.extend({
    data() {
        const params = new URLSearchParams(window.location.search);
        return {
            access_token: params.get("access_token"),
        }
    },
    methods: {
        save() {
            const file = document.getElementById("file").files[0];
            const reader = new FileReader();
            reader.onerror = (e) => {
                new Noty({
                    text: "<b>failed to load the file (check out developer tools)</b>",
                    type: "error",
                }).show();
                console.error(e);
            }

            reader.onload = (e) => {
                this.upload(convert(e.target.result));
            }

            reader.readAsText(file);
        },
        upload(body: string) {
            request
                .post("/api/save_history")
                .set("Content-Type", "application/octet-stream")
                .query({ access_token: this.access_token })
                .send(body)
                .then(() => {
                    new Noty({
                        text: "<b>saved to ~/.nsh_history</b>",
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
        }
    }
})
</script>
