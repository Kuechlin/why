run:
	cargo run

wasm:
	wasm-pack build --target web

web:
	cd web && npm run dev