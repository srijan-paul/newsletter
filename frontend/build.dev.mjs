import esbuild from "esbuild"
import pursPlugin from "esbuild-plugin-purescript"
import copy from "esbuild-plugin-copy"

const ctx = await esbuild
  .context({
    entryPoints: ["src/index.js"],
    bundle: true,
    outdir: "dist",
    plugins: [
      // allow importing Purescript modules in JavaScript files.
      pursPlugin(),
      // copy everything under `static` to `dist`.
      copy({
        // this is equal to process.cwd(), which means we use cwd path as base path to resolve `to` path
        // if not specified, this plugin uses ESBuild.build outdir/outfile options as base path.
        resolveFrom: 'cwd',
        assets: {
          from: ['./static/*'],
          to: ['./dist'],
        },
        watch: true,
      }),
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

