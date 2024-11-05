<script setup lang="ts">
import { useRouter } from 'vue-router';
import { ref } from 'vue';
import { getImageList, getSimilarList, downloadJson } from '@/http-api';
import { ImageType, SimilarType } from '@/image';

const router = useRouter();
const imageList = ref<ImageType[]>([]);
const similarList = ref<SimilarType[]>([]);

const id = Number(router.currentRoute.value.params.id);
const histo = Number(router.currentRoute.value.query.histo);
const many = Number(router.currentRoute.value.query.many);

getImageList(imageList);
getSimilarList(similarList, id, histo, many);
</script>

<template>
    <div>
        <h2>Images List Similar</h2>
    </div>
    <div>
        <p>Similarity for :</p>
        <div class="textbox">
            <p>Image: {{ imageList[imageList.findIndex((element) => element.id === id)].name }}</p>
            <p>id: {{ imageList[imageList.findIndex((element) => element.id === id)].id }}</p>
            <p>type: {{ imageList[imageList.findIndex((element) => element.id === id)].type }}</p>
            <p>size: {{ imageList[imageList.findIndex((element) => element.id === id)].size }}</p>
            <p v-if="histo == 1">Histo: 2D-GL</p>
            <p v-if="histo == 2">Histo: 2D-HS</p>
            <p v-if="histo == 3">Histo: 3D-RGB</p>
            <p>Number : {{ many }}</p>
        </div>
    </div>

    <div>
        <table>
            <thead>
                <tr>
                    <th scope="col">ID</th>
                    <th scope="col">Name</th>
                    <th scope="col">Type</th>
                    <th scope="col">Size</th>
                    <th scope="col">Score</th>
                    <th scope="col">Likes</th>
                </tr>
            </thead>
            <tbody>
                <tr v-for="image in similarList" :key="image.id">
                    <th scope="row">{{ image.id }}</th>
                    <td>{{ image.name }}</td>
                    <td>{{ image.type }}</td>
                    <td>{{ image.size }}</td>
                    <td>{{ image.score }}</td>
                    <td>{{ image.likes }}</td>
                </tr>
            </tbody>
        </table>
        <button
            v-on:click="downloadJson(`/images/similar/${id}?many=${many}&histo=${histo}`, `Similar_${id}_${histo}_${many}`)">
            <img src="../assets/save.png" alt="save" class="buttonicon">
        </button>
    </div>
</template>
<style scoped>
.textbox {
    margin-bottom: 2em;
}

table {
    margin-bottom: 1em;
}
</style>