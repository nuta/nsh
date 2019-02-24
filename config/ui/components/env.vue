<template>
<div>
    <h2>PATH</h2>
    <label for="env">The executable directories separated by <code>:</code> (colon).</label>
    <div class="input-group mb-3">
        <textarea id="env" type="text" class="form-control" v-model="path"></textarea>
    </div>
</div>
</template>

<script lang="ts">
export default {
    props: ["path"],
    watch: {
        path() {
            this.$emit("update:path", this.path);
        }
    }
}
</script>
