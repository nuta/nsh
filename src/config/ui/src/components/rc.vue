<template>
<div>
    <h2>Startup Script</h2>
    <label for="rc">Startup Script (known as <code>.bashrc</code> in Bash)</label>
    <div class="input-group mb-3">
        <div id="rc-editor" type="text" class="editor"></div>
    </div>
</div>
</template>

<script lang="ts">
export default {
    props: ["script"],
    watch: {
        script() {
            /// XXX: We have to wait for the GET /api/load request to be completed.
            const editor = ace.edit("rc-editor");
            if editor.session.getValue().length == 0 {
                editor.session.setValue(this.script);
            }
        }
    },
    mounted() {
        // Initialize Ace editor.
        const editor = ace.edit("rc-editor");
        editor.setTheme("ace/theme/monokai");
        editor.setShowPrintMargin(false);
        editor.session.setMode("ace/mode/sh");
        editor.session.on('change', () => {
            this.$emit("changed", editor.session.getValue());
        });
    }
}
</script>

<style lang="scss" scoped>

.editor {
    position: relative;
    height: 400px;
    width: 100%;
}

</style>
