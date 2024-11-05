# PDL - l1b

Ce projet, IMGine, est une application web conçue pour le stockage et le traitement de photos au format PNG et JPEG. Il offre également des fonctionnalités de recherche par similarité pour retrouver facilement vos images préférées.

### Technologies

[![Java](https://img.shields.io/badge/Java_17-ED8B00?style=for-the-badge&logo=openjdk&logoColor=white)](https://www.java.com/fr/) [![PostgreSQL](https://img.shields.io/badge/PostgreSQL_42-336791?style=for-the-badge&logo=postgresql&logoColor=white)](https://www.postgresql.org/) [![Vue.js](https://img.shields.io/badge/Vue.js-4FC08D?style=for-the-badge&logo=vue-dot-js&logoColor=white)](https://vuejs.org/)  [![TypeScript](https://img.shields.io/badge/TypeScript-007ACC?style=for-the-badge&logo=typescript&logoColor=white)](https://www.typescriptlang.org/) 

[![Maven](https://img.shields.io/badge/Maven-C71A36?style=for-the-badge&logo=apache-maven&logoColor=white)](https://maven.apache.org/) [![Spring Boot](https://img.shields.io/badge/Spring_Boot-6DB33F?style=for-the-badge&logo=spring-boot&logoColor=white)](https://spring.io/projects/spring-boot) [![BoofCV](https://img.shields.io/badge/BoofCV-6CBB5A?style=for-the-badge)](https://boofcv.org/) [![PgVector](https://img.shields.io/badge/PgVector-336791?style=for-the-badge&logo=postgresql&logoColor=white)](https://example.com)



## Pour lancer le projet
### Installer:
Pour commancer, télécharger le projet, avec **GIT** (clone) :
- Avec SSH : `git@gitlab.emi.u-bordeaux.fr:pdl-l3/teams/2024/l1/l1b.git`


Dans le terminal, à la racine du projet :
```
mvn clean install compile
mvn --projects backend spring-boot:run
```
Dans un navigateur (de préférence Chrome, certaines fonctionnalités ne marchent pas correctement sur Firefox), entrer l'url [suivante](localhost:8765) (localhost:8765).

### Configuration accès à la base de données:
Pour le bon fonctionnement du serveur, merci de définir les variables d'environnement DATABASE_NAME et DATABASE_PASSWORD avec vos username et password (dans un terminal ou dans le fichier .bashrc)
```
export DATABASE_NAME=username
export DATABASE_PASSWORD=password
```

## Compatibilité :
Le serveur a été testé sur :
### Systèmes d'exploitation
[![Ubuntu](https://img.shields.io/badge/Ubuntu-E95420?style=for-the-badge&logo=ubuntu&logoColor=white)](https://ubuntu.com/) [![Kubuntu](https://img.shields.io/badge/Kubuntu-0079c1?style=for-the-badge&logo=kubuntu&logoColor=white)](https://kubuntu.org/) [![Windows 10](https://img.shields.io/badge/Windows_11-0066CC?style=for-the-badge&logo=windows&logoColor=white)](https://www.microsoft.com/windows/)
### Navigateurs
[![Google Chrome](https://img.shields.io/badge/Google_Chrome-4285F4?style=for-the-badge&logo=google-chrome&logoColor=white)](https://www.google.com/chrome/) [![Microsoft Edge](https://img.shields.io/badge/Microsoft_edge-46d482?style=for-the-badge&logo=microsoft-edge&logoColor=white)](https://www.microsoft.com/fr-fr/edge/download?form=MA13FJ)
## Travaux

### Besoins : 
Ce serveur répond aux différents besoins suivants :
- Initialiser un ensemble d’images présentes sur le serveur
- Gérer les images présentes sur le serveur
- Indexer une image
- Rechercher des images similaires à une image donnée
- Transférer la liste des images existantes
- Ajouter une image
- Récupérer une image
- Supprimer une image
- Transférer la liste des images les plus similaires à une image donnée
- Parcourir les images disponibles sur le serveur
- Sélectionner une image et afficher les images similaires
- Enregistrer une image sur disque
- Ajouter une image aux images disponibles sur le serveur
- Supprimer une image
- Intégration continue
- Compatibilité du serveur
- Compatibilité du client
- Documentation d’installation et de test

### Fonctionnalités ajoutées
- Compatibilité avec les images RGBA
- Compatibilité image en Noir et Blanc
- Recherche par similarité image Noir et Blanc (histo 1D gray level)
- Systeme de upvote et downvote (reddit)
- Traitement des images (avec différents filtres)
- Listes des images repensé (possibilité de télécharger le json)
- Interface utilisateur repensé (agréable, simple d'utilisation, ...)


## Contributeurs
- Nicolas Cornu--Laporte : nicolas.cornu-laporte@etu.u-bordeaux.fr
- Thomas Alexandre Moreau : thomas-alexandre.moreau@etu.u-bordeaux.fr
- Timothée Cohen : timothee.cohen@etu.u-bordeaux.fr