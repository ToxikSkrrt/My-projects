<script setup lang="ts">
  const Url = window.location.pathname;
</script>

<template>
  <div>
    <h2>Not Found</h2>
    <h3>Oops! It seems you're lost</h3>
    <p>The page "{{ Url }}" doesn't exist !</p>
    <p>Looks like you've taken a dead-end, take a break (have a KITKAT &#127851;)</p>
  </div>
</template>

<style></style>