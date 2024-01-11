// @ts-ignore
import { lezer } from "@lezer/generator/rollup";
import { resolve } from "path";
import { defineConfig } from "vite";

export default defineConfig({
    plugins: [{ ...lezer(), enforce: "pre" }],
    base: "/why/",
    server: {
        fs: {
            allow: [__dirname, resolve(__dirname, "..", "pkg")],
        },
    },
});
