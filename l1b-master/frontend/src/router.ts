import { createWebHistory, createRouter } from "vue-router";
import { RouteRecordRaw } from "vue-router";

const routes: Array<RouteRecordRaw> = [
  {
    path: "/",
    name: "home",
    component: () => import("./components/Home.vue"),
    props: true
  },
  {
    path: "/gallery",
    name: "gallery",
    component: () => import("./components/Gallery.vue"),
    props: true
  },
  {
    path: "/image/:id",
    name: "image",
    component: () => import("./components/Image.vue"),
    props: ({ params }) => ({ id: Number(params.id) || 0 })
  },
  {
    path: "/upload",
    name: "upload",
    component: () => import("./components/Upload.vue"),
    props: true
  },
  {
    path: "/similar",
    name: "similar",
    component: () => import("./components/Similar.vue"),
    props: true
  },
  {
    path: "/list",
    name: "list",
    component: () => import("./components/List.vue"),
    props: true
  },
  {
    path: "/list/similar/:id",
    name: "listSimilar",
    component: () => import("./components/ListSimilar.vue"),
    props: route => ({
      id: Number(route.params.id),
      histo: Number(route.query.histo),
      many: Number(route.query.many)
    })
  },
  {
    path: "/treatment",
    name: "treatment",
    component: () => import("./components/Treatment.vue"),
    props: true
  },
  {
    path: "/:pathMatch(.*)*",
    name: "NotFound",
    component: () => import("./components/NotFound.vue"),
    props: true
  }
];

const router = createRouter({
  history: createWebHistory(),
  routes,
});

export default router;