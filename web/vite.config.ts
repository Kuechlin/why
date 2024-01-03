import { resolve } from "path";
import { defineConfig } from "vite";

export default defineConfig({
    server: {
        fs: {
            allow: [__dirname, resolve(__dirname, "..", "pkg")],
        },
    },
});
