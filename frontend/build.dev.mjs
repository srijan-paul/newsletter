import esbuild from "esbuild"
import pursPlugin from "esbuild-plugin-purescript"
import copyStaticFiles from "esbuild-copy-static-files"

const ctx = await esbuild
  .context({
    entryPoints: ["src/index.js"],
    bundle: true,
    outdir: "dist",
    plugins: [
      // allow importing Purescript modules in JavaScript files.
      pursPlugin(),
      // copy everything under `static` to `dist`.
      copyStaticFiles({ src: "./static", dest: "./dist" })
    ],
    logLevel: "debug"
  })
  .catch((e) => {
    console.error(e)
    process.exit(1)
  });

// you can use a CLI flag for this, 
// instead of unconditionally calling `watch` every time.
await ctx.watch()
// same applies to `serve`.
await ctx.serve({ servedir: "./dist", port: 3000 })

