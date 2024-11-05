<script setup lang="ts">
  import { ref } from 'vue'
  import { getImageList } from '@/http-api';
  import { ImageType } from '@/image'
  import Image from './Image.vue';

  const imageList = ref<ImageType[]>([]);
  getImageList(imageList);
</script>

<template>

  <body>
    <div class="container">
      <h2>Gallery of {{ imageList.length }} photos</h2>
      <div v-if="imageList" class="grid">
        <Image class="img" v-for="image in imageList" :key="image.id" :id="image.id" />
      </div>
    </div>
  </body>
</template>

<style scoped>
.grid {
  display: flex;
  box-sizing: border-box;
  max-width: 100%;
  justify-content: space-evenly;
  margin: 0;
  flex-wrap: wrap;
  padding: 0;
}

.grid:deep(figure img) {
  width: 350px;
  overflow: auto;
  border: 5px solid rgba(137, 43, 226, 0.421);
  border-radius: 8px;
  aspect-ratio: 1/1;
  margin-top: 10px;
  box-shadow: 0 2px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19);
}
</style>
