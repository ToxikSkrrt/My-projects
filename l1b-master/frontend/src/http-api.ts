import axios, { AxiosResponse, AxiosError } from 'axios';
import { ImageType, SimilarType } from '@/image';
import { Ref } from 'vue';
import { validateHeaderName } from 'http';

const instance = axios.create({
  baseURL: "/",
  timeout: 15000,
});

const responseBody = (response: AxiosResponse) => response.data;

const requests = {
  get: (url: string, param: {}) => instance.get(url, param).then(responseBody),
  post: (url: string, body: {}) => instance.post(url, body, { headers: { "Content-Type": "multipart/form-data" }, }).then(responseBody),
  put: (url: string, body: {}) => instance.put(url, body).then(responseBody),
  delete: (url: string) => instance.delete(url).then(responseBody)
};

export const api = {
  getImageList: (): Promise<ImageType[]> => requests.get('images', {}),
  getImage: (id: number): Promise<Blob> => requests.get(`images/${id}`, { responseType: "blob" }),
  getTreatment: (id: number, filter: string, value: string): Promise<Number> => requests.get(`images/treatment/${id}`, { params: { filter: filter, value: value }}),
  getSimilar: (id: number, histo: string, many: string): Promise<SimilarType[]> => requests.get(`images/similar/${id}`, { params: { many: many, histo: histo }}),
  createImage: (form: FormData): Promise<ImageType> => requests.post('images', form),
  deleteImage: (id: number): Promise<void> => requests.delete(`images/${id}`),
  upVote: (name: string, id: number): Promise<void> => requests.get(`images/upVote/${name}/${id}`, {}),
  downVote: (name: string, id: number): Promise<void> => requests.get(`images/downVote/${name}/${id}`, {})
};

export function getTreatment(TreatmentID: Ref<Number>, id: number, filter: number, value:number) {
  api.getTreatment(id, String(filter), String(value)).then((data) => {
    TreatmentID.value = data;
  }).catch(e => {
    console.log(e.message);
  });
}

export function getImageList(imageList: Ref<ImageType[]>) {
  api.getImageList().then((data) => {
    imageList.value = data;
  }).catch(e => {
    console.log(e.message);
  });
}

export function getImageListColor(imageListColor: Ref<ImageType[]>) {
  api.getImageList().then((data) => {
    imageListColor.value = data.filter((image: { isgray: boolean }) => !image.isgray);
  }).catch(e => {
    console.log(e.message);
  });
}

export function getImageListGray(imageListColor: Ref<ImageType[]>) {
  api.getImageList().then((data) => {
    imageListColor.value = data.filter((image: { isgray: boolean }) => image.isgray);
  }).catch(e => {
    console.log(e.message);
  });
}

export function getSimilarList(similarList: Ref<SimilarType[]>, id: number, histo: number, many: number) {
  api.getSimilar(id, String(histo), String(many)).then((data) => {
		similarList.value = data;
	})
	.catch(e => {
		console.log(e.message);
	});
}

export function downloadFile(url: string, filename: string): void {
  const link = document.createElement('a');
  link.href = url;
  link.target = '_blank';
  link.download = filename;

  link.click();
}

export function downloadJson(url: string, filename: string): void {
  axios.get(url)
    .then(response => {
      const data = response.data;
      const json = JSON.stringify(data);
      const blob = new Blob([json], { type: 'application/json' });
      const lien = URL.createObjectURL(blob);
      const link = document.createElement('a');
      link.href = lien;
      link.download = filename;
      link.click();
    })
    .catch(error => {
      console.error('Error downloading JSON:', error);
    });
}

export function deleteImage(id: number): void {
  api.deleteImage(id);
  location.reload();
}

export function upVote(name: string, id: number) {
  api.upVote(name, id);
}

export function downVote(name: string, id: number) {
  api.downVote(name, id);
}