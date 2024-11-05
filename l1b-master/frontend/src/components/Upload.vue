<script setup lang="ts">
  import { ref } from 'vue';
  import { api, getImageList } from '@/http-api';
  import { ImageType } from '@/image';

  const target = ref<HTMLInputElement>();
    
  const imageList = ref<ImageType[]>([]);
  getImageList(imageList);

  const errorMessage = ref<string>('');

  function submitFile() {
    if (target.value !== null && target.value !== undefined && target.value.files !== null) {
      const file = target.value.files[0];
      if (file === undefined){
        console.log('Aucunne image n\'est téléversée.');
        errorMessage.value = 'Aucunne image n\'est téléversée.';
        return;
      } 
      if (!file.type.startsWith('image/png') && !file.type.startsWith('image/jpeg')) {
        console.log('Le type de fichier n\'est pas autorisé. Seuls les fichiers PNG et JPEG(JPG) sont autorisés.');
        errorMessage.value = 'Le type de fichier n\'est pas autorisé. Seuls les fichiers PNG et JPEG(JPG) sont autorisés.';
        return;
      }
      const fileName = file.name;
      const isDuplicateName = imageList.value.some(image => image.name === fileName);
      if (isDuplicateName) {
        console.log('Un fichier avec le même nom existe déjà. Veuillez choisir un nom de fichier différent.');
        errorMessage.value = 'Un fichier avec le même nom existe déjà. Veuillez choisir un nom de fichier différent.';
        return;
      }
      let formData = new FormData();
      formData.append("file", file);
      api.createImage(formData).then(() => {
        if (target.value !== undefined)
          target.value.value = '';
      }).catch(e => {
        console.log(e.message);
      });
    }
  }

  function handleFileUpload(event: Event) {
    target.value = (event.target as HTMLInputElement);
  }
</script>

<template>

  <body>
    <div>
      <h2>Upload an image</h2>
      <p>Load the image and send it to everyone to see</p>
    </div>
    <div>
      <div>
        <input type="file" id="file" ref="file" @change="handleFileUpload" accept=".jpg,.jpeg,.png" />
      </div>
      <div>
        <button @click="submitFile">Submit</button>
      </div>
      <div class="error-message" v-if="errorMessage">{{ errorMessage }}</div>
    </div>
  </body>
</template>

<style scoped>
.error-message {
  color: red;
}
</style>
