<script setup lang="ts">
import { computed, ref } from 'vue';
import { getImageList, getSimilarList, api } from '@/http-api';
import { ImageType, SimilarType } from '@/image';
import Image from './Image.vue';

const imageList = ref<ImageType[]>([]);
const imageListColor = ref<ImageType[]>([]);
const similarList = ref<SimilarType[]>([]);

const click = ref(false);

const selectedId = ref(-1);
const selectedHisto = ref(-1);
const selectedMany = ref(1);


getImageList(imageList);

function Similar() {
  selectedMany.value = parseInt((document.getElementById("many") as HTMLInputElement).value);
  click.value = true;
  similarList.value = [];
  getSimilarList(similarList, selectedId.value, selectedHisto.value, selectedMany.value);
}

function isAllInfoEntered() {
  return selectedId.value !== -1 && selectedHisto.value !== -1;
}

const linkSimilar = () => {
  return `/list/similar/${selectedId.value}?histo=${selectedHisto.value}&many=${selectedMany.value}`;
}

const isGrayScale = computed(() => {
  const index = imageList.value.findIndex((element) => element.id === selectedId.value);
  return index !== -1 && imageList.value[index].isgray;
});

const max = computed(() => {
  if (selectedHisto.value === 1) {
    return imageList.value.filter(image => image.isgray).length - 1;
  } else {
    return imageList.value.filter(image => !image.isgray).length - 1;
  }
});
</script>

<template>
  <div>
    <h2>Find similar images </h2>
    <p>Choose the source image, the comparison histogram and the number of similar images you want </p>
    <p>If you wish, you can retrieve the list afterwards</p>
  </div>
  <div>
    <div>
      <select v-model="selectedId">
        <option disabled value="">Select a picture</option>
        <option v-for="image in imageList" :value="image.id" :key="image.id">{{ image.name }}</option>
      </select>
      <select v-model="selectedHisto">
        <option disabled value="">Select a histogram</option>
        <option :value="1" :disabled="!isGrayScale">1D Gray Level</option>
        <option :value="2" :disabled="isGrayScale">2D Hue/Saturation</option>
        <option :value="3" :disabled="isGrayScale">3D Red/Green/Blue</option>
      </select>
      <input type="number" id="many" placeholder="Select number" min="1" :max="max" />
      <br>
      <button @click="Similar" :disabled="!isAllInfoEntered()">Done</button>

    </div>
    <div class="parent">
      <div>
        <div class="imagebox">
          <img v-if="selectedId != -1" :src="`images/${selectedId}`" />
        </div>
      </div>

      <div>
        <div v-if="similarList.length >= 1" id="similar">
          <h3>The {{ selectedMany }} most similar image(s):</h3>
          <div v-for="image in similarList" class="imagebox">
            <Image :id="image.id"></Image>
            <p>diffscore:
            <div class="number"> {{ image.score }} </div>
            </p>
          </div>
          <button>
            <router-link v-if="click" :to="linkSimilar()">List Similar</router-link>
          </button>

        </div>
      </div>
    </div>
  </div>
</template>

<style scoped></style>
