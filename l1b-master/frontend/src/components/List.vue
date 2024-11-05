<script setup lang="ts">
import { ref } from 'vue';
import { getImageList, downloadJson } from '@/http-api';
import { ImageType } from '@/image';

const imageList = ref<ImageType[]>([]);
getImageList(imageList);

const showGray = ref(true);
const showColor = ref(true);

function filteredImageList() {
    // Filtrer les images en fonction de l'état des cases à cocher
    return (imageList.value as ImageType[]).filter((element) => {
        if (showGray.value && showColor.value) {
            return true; // Afficher toutes les images
        } else if (showGray.value) {
            return element.isgray === true; // Afficher seulement les images grises
        } else if (showColor.value) {
            return element.isgray === false; // Afficher seulement les images en couleur
        }
        return false; // Ne rien afficher si aucune case n'est cochée
    });
}
</script>

<template>
    <div>
        <h2>Images List</h2>
        <p>List of all the images on the server</p>
        <p>You can download the corresponding json file</p>
        <!-- Ajouter des cases à cocher pour filtrer les images par couleur -->
        <label>
            <input type="checkbox" v-model="showGray"> Gray
        </label>
        <label>
            <input type="checkbox" v-model="showColor"> Color
        </label>
    </div>
    <button v-on:click="downloadJson(`images`, `List_images`)">
        <img src="../assets/save.png" alt="save" class="buttonicon">
    </button>
    <table>
        <thead id="tabletop">
            <tr>
                <th scope="col">ID</th>
                <th scope="col">Name</th>
                <th scope="col">Type</th>
                <th scope="col">Size</th>
                <th scope="col">Likes</th>
            </tr>
        </thead>
        <tbody>
            <tr v-for="image in filteredImageList()" :key="image.id">
                <th scope="row">{{ image.id }}</th>
                <td> {{ image.name }}</td>
                <td>{{ image.type }}</td>
                <td>{{ image.size }}</td>
                <td>{{ image.likes }}</td>
            </tr>
        </tbody>
        <tfoot>
            <tr>
                <th scope="row" colspan="2">Total</th>
                <td>{{ filteredImageList().length }}</td>
            </tr>
        </tfoot>
    </table>
    <p id="total">Total: {{ filteredImageList().length }} images</p>

</template>

<style scoped>
#total {
    font-weight: bold;
}
</style>
