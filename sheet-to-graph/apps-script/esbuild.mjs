import esbuild from "esbuild";

await esbuild.build({
  entryPoints: ["src/main.ts"],
  bundle: true,
  outfile: "dist/Code.gs",
  target: ["es2019"],
  format: "iife"
});