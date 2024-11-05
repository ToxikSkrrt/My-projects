<script setup lang="ts">
</script>

<template>
  <div id="app">
    <header>
      <router-link to="/" id="logo">
        <img src="/src/assets/logo.png" alt="Logo" />
      </router-link>
      <nav class="navbar">
        <router-link to="/" class="navbutton">Home</router-link>
        <router-link to="/gallery" class="navbutton">Gallery</router-link>
        <router-link to="/upload" class="navbutton">Upload</router-link>
        <router-link to="/similar" class="navbutton">Similar</router-link>
        <router-link to="/treatment" class="navbutton">Treatment</router-link>
        <router-link to="/list" class="navbutton">List</router-link>
      </nav>
    </header>
    <div>
      <h1 id="title">IMGine</h1>
      <router-view />
    </div>
  </div>
</template>

<style>
body {
  padding-top: 80px;
  background-color: white;
  color: black;
}

* {
  box-sizing: border-box;
  text-align: center;
  font-family: 'Courier New', Courier, monospace;
}

/* Nav Bar */
header {
  margin: 0;
  padding: 0;
  text-align: center;
  position: fixed;
  top: 0;
  left: 0;
  width: 100%;
  padding: 10px 50px;
  background: rgba(137, 43, 226, 0.819);
  backdrop-filter: blur(1px);
  display: flex;
  justify-content: space-between;
  align-items: center;
  z-index: 100;
  border-radius: 20px;
  box-shadow: 0 2px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19);
}

#logo img {
  height: 60px;
}

.navbar router-link,
.navbar a {
  position: relative;
  font-size: 22px;
  color: white;
  font-weight: 500;
  text-decoration: none;
  margin-left: 30px;
}

.navbar router-link::before,
.navbar a:before {
  content: '';
  position: absolute;
  top: 100%;
  left: 0;
  width: 0;
  height: 2px;
  background: #fff;
  transition: .3s;
}

.navbar router-link:hover::before,
.navbar a:hover::before {
  width: 100%;
}

#title {
  color: blueviolet;
  font-size: 50px;
  text-shadow: 1px 1px 4px #aaa;
}


/* Boutons et sélecteurs */

select,
button,
input[type=file]::file-selector-button {
  color: blueviolet;
  background-color: rgb(247, 236, 255);
  border-radius: 8px;
  border-color: rgba(137, 43, 226, 0.421);
  border-width: 3px;
  margin: 10px 5px;
  box-shadow: 0 1px 5px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19);
}

input[type=number] {
  width: 6em;
  border-radius: 8px;
  border-color: rgba(137, 43, 226, 0.421);
  border-width: 3px;
  margin: 10px 5px;
  box-shadow: 0 1px 5px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19);
}
button:hover {
  background-color: rgb(248, 241, 255);
  cursor: pointer;
}

button:active {
  background-color: rgb(222, 187, 255);
  transform: translateY(1px)
}

.buttonicon {
  height: 35px;
  margin: 0;
}
input[type=file] {
  background-color: rgba(137, 43, 226, 0.124);
  border-radius: 8px;
  border-color: rgba(137, 43, 226, 0.421);
  border-width: 3px;
  width: 15%;
min-width: 25em;
}

/* Gestion des tableaux */
table {
  border: 5px solid rgba(137, 43, 226, 0.421);
  border-radius: 8px;
  background-color: rgba(137, 43, 226, 0.062);
  width: 80%;
  max-width: 60em;
  position: relative;
  margin: auto;
  box-shadow: 0 2px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19);
}

thead {
  background-color: rgba(137, 43, 226, 0.222);
  border: 0;
}

/* Zones de textes*/
.textbox {
  border: solid rgba(137, 43, 226, 0.421);
  border-radius: 8px;
  max-width: 20em;
  margin: 0 auto;
  margin-top: 30px;
  background-color: rgba(137, 43, 226, 0.024);
  box-shadow: 0 2px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19);
  line-height: 1em;
}
/* Boîtes images */
.imagebox img {
  max-width: 40%;
  height: auto;
  margin: auto;
  overflow: hidden;
  border: solid rgba(137, 43, 226, 0.421);
  border-radius: 8px;
  aspect-ratio: auto;
  margin-top: 10px;
  box-shadow: 0 2px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19);
}

.imagebox img {
  width: 100%;
  height: 100%;
  object-fit: contain;
}

.number {
  display: inherit;
  height: 40px;
  width: 40px;
  line-height: 40px;
  border-radius: 50%;
  box-shadow: 0 2px 8px 0 rgba(0, 0, 0, 0.2), 0 6px 20px 0 rgba(0, 0, 0, 0.19);
  background-color: blueviolet;
  color: white;
  font-weight: bold;
  margin: auto;
}
</style>
