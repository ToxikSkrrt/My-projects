<script setup lang="ts">
import { Ref, computed, ref, watch, watchEffect, nextTick } from 'vue';
import router from "@/router";
import { getImageList, downloadFile, deleteImage, upVote, downVote } from '@/http-api';
import { ImageType } from '@/image';
import Image from './Image.vue';
import axios from 'axios';


const selectedId = ref(-1);
const imageList = ref<ImageType[]>([]);
const triggerUpdateVotes = ref<number>(0);

function updateLikes() {
  getImageList(imageList);

  const currentImage = imageList.value.find(image => image.id === selectedId.value);
  return currentImage ? currentImage.likes : 0;
}

watch([triggerUpdateVotes], () => {
  const likes = updateLikes();
  const container = document.getElementById('div_likes')
  if (container)
    container.innerHTML = `Likes : ${likes}`;
});

function dualCall(name: string, id: number, choice: number) {
  if (choice === 0) {
    upVote(name, id);
  }
  else {
    downVote(name, id);
  }
  triggerUpdateVotes.value = triggerUpdateVotes.value;
}

getImageList(imageList);
</script>

<template>
  <div>
    <h1>Have fun with your images & your IMGination &#10024;</h1>
    <h2>Choose an image</h2>
    <p>Select, view, download or delete an image</p>
    <div>
      <select v-model="selectedId">
        <option disabled value="">Select a picture</option>
        <option v-for="image in imageList" :value="image.id" :key="image.id">{{ image.name }}</option>
      </select>
    </div>
    <div>
      <div id="parent">
        <div id="likes">
          <button v-if="selectedId != -1"
            v-on:click="dualCall(imageList[imageList.findIndex((element) => element.id === selectedId)].name, imageList[imageList.findIndex((element) => element.id === selectedId)].id, 0)"
            id="upvote">
            <img src="../assets/up.png" alt="upvote" class="buttonicon">
          </button>
          <div v-if="selectedId != -1" id="div_likes" class="number">{{ updateLikes() }}
          </div>
          <button v-if="selectedId != -1"
            v-on:click="dualCall(imageList[imageList.findIndex((element) => element.id === selectedId)].name, imageList[imageList.findIndex((element) => element.id === selectedId)].id, 1)"
            id="downvote">
            <img src="../assets/down.png" alt="downvote" class="buttonicon">
          </button>
        </div>
        <div v-if="selectedId != -1" class="imagebox">
          <img :src="`images/${selectedId}`" />
        </div>
        <div id="savedelete">
          <button v-if="selectedId != -1"
            v-on:click="downloadFile(`images/${selectedId}`, imageList[imageList.findIndex((element) => element.id === selectedId)].name)">
            <img src="../assets/save.png" alt="save" class="buttonicon">
          </button>
          <br><br>
          <button v-if="selectedId != -1" v-on:click="deleteImage(selectedId)">
            <img src="../assets/delete.png" alt="delete" class="buttonicon">
          </button>
        </div>
      </div>
      <div v-if="selectedId != -1" class="textbox">
        <p>Name : {{ imageList[imageList.findIndex((element) => element.id === selectedId)].name }}</p>
        <p>ID : {{ imageList[imageList.findIndex((element) => element.id === selectedId)].id }}</p>
        <p>Type : {{ imageList[imageList.findIndex((element) => element.id === selectedId)].type }}</p>
        <p>Size : {{ imageList[imageList.findIndex((element) => element.id === selectedId)].size }}</p>
      </div>
    </div>
    <br>
  </div>
</template>

<style scoped>
#parent {
  display: flex;
  align-items: center;
  justify-content: center;
}



button {
  margin-left: 3em;
  margin-right: 3em;
}

.imagebox {
  max-width: 70%;
  height: auto;
}

.imagebox img {
  max-width: 100%;
  height: auto;
  margin: auto;
  overflow: hidden;
  border: solid rgba(137, 43, 226, 0.421);
  border-radius: 8px;
  aspect-ratio: auto;
  margin-top: 10px;
  box-shadow: 0 2px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19);
}
</style>
