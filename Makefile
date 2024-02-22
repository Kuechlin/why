run:
	cd why && cargo run

file:
	cd why && cargo run ../tests/file.y

dev:
	cd web && npm run dev

wasm:
	cd why && wasm-pack build --target web

website:
	cd web && npm run build

build: wasm website