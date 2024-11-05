<script setup lang="ts">
import { ref } from 'vue';
import { getImageList, getTreatment, downloadFile, deleteImage } from '@/http-api';
import { ImageType } from '@/image';


const imageList = ref<ImageType[]>([]);
getImageList(imageList);

const TreatmentID = ref(-1);

const selectedId = ref(-1);
const selectedFilter = ref(-1);
const selectedValue = ref(1);

const click = ref(false);


async function refreshImageList() {
    await getImageList(imageList);
}

async function TreatmentWithValue(){
    selectedValue.value = parseInt((document.getElementById("value") as HTMLInputElement).value);
    click.value = true;
    getTreatment(TreatmentID, selectedId.value, selectedFilter.value, selectedValue.value);
    await refreshImageList();
}

async function TreatmentWithoutValue(){
    click.value = true;
    getTreatment(TreatmentID, selectedId.value, selectedFilter.value, selectedValue.value);
    await refreshImageList();
}

</script>

<template>
    <div>
        <h2>Image treatment</h2>
        <p>Choose the source image, the filter and values you want </p>
        <p>If you wish, you can download the result or push on the server</p>
    </div>
    <div>
        <div>
            <select v-model="selectedId">
                <option disabled value="">Select a picture</option>
                <option v-for="image in imageList" :value="image.id" :key="image.id">{{ image.name }}</option>
            </select>
            <br>
            <div v-if="selectedId != -1" class="imagebox">
                <img :src="`images/${selectedId}`" />
            </div>
        </div>
        <br>
        <div v-if="selectedId != -1">
            <select v-model="selectedFilter">
                <option disabled value="Filtre">Select filter</option>
                <option v-if="imageList[imageList.findIndex((element) => element.id === selectedId)].isgray" :value="1">
                    Threshold</option>
                <option v-if="imageList[imageList.findIndex((element) => element.id === selectedId)].isgray" :value="2">
                    Contrast</option>
                <option v-if="imageList[imageList.findIndex((element) => element.id === selectedId)].isgray" :value="3">
                    Equalization</option>
                <option :value="4">Brightness</option>
                <option :value="5">Mean Filter</option>
                <option v-if="imageList[imageList.findIndex((element) => element.id === selectedId)].isgray" :value="6">
                    Outline Detect</option>
                <option v-if="!imageList[imageList.findIndex((element) => element.id === selectedId)].isgray"
                    :value="7">Color To Gray</option>
                <option v-if="!imageList[imageList.findIndex((element) => element.id === selectedId)].isgray"
                    :value="8">Color Filter</option>
            </select>
            <div v-if="selectedFilter == 1 || selectedFilter == 4">
                <input type="range" id="value" name="Gray" min="0" max="255" />
                <button @click="TreatmentWithValue">Apply</button>
            </div>
            <div v-if="selectedFilter == 5">
                <input type="range" id="value" name="Gray" min="0" max="40" step="4" />
                <button @click="TreatmentWithValue">Apply</button>
            </div>
            <div v-if="selectedFilter == 8">
                <input type="range" id="value" name="Hue" min="0" max="359" />
                <button @click="TreatmentWithValue">Apply</button>
            </div>
            <div v-if="selectedFilter == 2 || selectedFilter == 3 || selectedFilter == 6 || selectedFilter == 7">
                <button @click="TreatmentWithoutValue">Apply</button>
            </div>
        </div>
    </div>
    <br>
    <div v-if="TreatmentID != -1" class="imagebox">
        <img :src="`images/${TreatmentID}`" />
        <p>The image is automatically stored on the server</p>
        <p>You can delete it if you don't like it !</p>
    </div>
    <div v-if="TreatmentID != -1">
        <button v-on:click="deleteImage(TreatmentID)">
            <img src="../assets/delete.png" alt="delete" class="buttonicon">
        </button>
    </div>
</template>

<style scoped>
.buttonicon {
    height: 35px;
    margin: 0;
}
</style>
