# Startup Landing Page

A starter template for very simple projects (no frameworks) with Tailwind CSS setup as a PostCSS plugin. You only need to install NPM.

Follow the Getting Started guide and build websites with Tailwind CSS. Also, follow the steps to Optimize for Production to end up with a very tiny final CSS bundle.

## Getting Started

1.  Download and install NPM - [A Beginner’s Guide to npm](https://www.sitepoint.com/npm-guide/)

2.  Install dependencies

        npm install

3.  Build using Tailwind CSS

        npm run build

4.  Open the `public > index.html` file in your browser and you should see a heading styled with a gradient. If you don't see a gradient on the text, something went wrong.

## How to use

- Go to `public > index.html` Remove the `<h1>` element and start adding your own HTML.
- If you need to add more HTML pages, add them in the `public` folder.
- To extract classes and use the `@apply` directive, edit the custom CSS file in `src > styles.css`. Add any amount of custom CSS within this file. Refer [https://tailwindcss.com/docs/installation#using-a-custom-css-file](https://tailwindcss.com/docs/installation#using-a-custom-css-file)

Watch HTML files for changes and build automatically everytime using

    npm run watch

NOTE: Do NOT edit the file `public > dist > styles.css` directly - This is the distribution stylesheet. The CSS here is generated from `src > styles.css` using Tailwind when you build.

## Optimize for production

Before pushing your code (the `public` folder) for production, run the below command to reduce the size of `styles.css` within the public folder

     npm run prod

NOTE: If you are using Windows and face an error `NODE ENV not recognised`, run the below command

     npm install win-node-env

## Updating dependencies and the nix build

Create a package-lock.json with `npm -i --package-lock-only`. Then run `node2nix`. Then remove the
created package-lock.json again.

## Development dependencies

The development dependencies have been moved to a separate stanza under "comment" in package.json to
avoid interfering with node2nix. For development, you need to uncomment these.
