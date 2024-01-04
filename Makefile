run:
	cargo run

wasm:
	wasm-pack build --target web

dev:
	cd web && npm run dev