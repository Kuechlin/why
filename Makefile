run:
	cargo run

file:
	cargo run ./tests/file.y

dev:
	cd web && npm run dev

wasm:
	wasm-pack build --target web

website:
	cd web && npm run build

build: wasm website