# Arcane Duelists (Gamedev)

Arcane Duelists là một dự án game hành động góc nhìn từ trên xuống (top‑down) viết bằng Haskell, theo kiến trúc client–server. Monorepo gồm 3 package: `client`, `server`, `shared`. Client render bằng SDL2; server authoritative xử lý logic, AI, vật lý, combat và đồng bộ trạng thái tới client.

## 1. Kiến Trúc Hệ Thống
- Monorepo quản lý bởi Stack, gồm 3 package: `client`, `server`, `shared`.
- Server authoritative: mọi quyết định gameplay ở server, client mỏng (thin client).
- Đồng thời (concurrency): dùng `STM` (TVar/TQueue) + `forkIO` để tách game loop, xử lý kết nối.
- Song song (parallel): dùng `Control.Parallel.Strategies` (`parList rdeepseq`) cho cập nhật projectile.
- Giao tiếp mạng: WebSocket (`network-websockets`).

### 1.1 Server
- Game loop cố định ~60Hz: xử lý Input → Physics → Combat → AI → Broadcast.
- Networking: WebSocket tại `127.0.0.1:9160` (xem `server/app/Main.hs`).
- Concurrency & Sync: `STM` bảo vệ `World`, hàng đợi input, danh sách client; mỗi client có handler riêng.
- Parallel: `Systems.PhysicsSystem.tick` dùng `parList rdeepseq` để cập nhật projectile theo lô.
- Trạng thái: `World = GameState` (trong `shared`), lưu người chơi, quái, đạn, item, hẹn giờ respawn.

### 1.2 Client
- Thin client: thu thập input, hiển thị trạng thái từ server, giữ rất ít logic gameplay.
- Rendering: SDL2, dữ liệu hoạt ảnh nạp từ thư mục `assets/textures` (người chơi, quái, UI).
- Resource loading: gom tất cả sprites vào `ResourceMap` (xem `client/src/Renderer/Resources.hs`).
- Map: nạp JSON map đơn giản từ `assets/maps` (xem `Systems/MapLoader.hs`).

### 1.3 Shared
- Đồng bộ kiểu dữ liệu giữa server và client: `Player`, `Enemy`, `Projectile`, `GameState`, `Protocol`.
- `Types.Config` chứa hằng số gameplay (tốc độ, sát thương, thời gian hồi…), tránh hard‑code rải rác.
- `NFData` đã được derive cho các kiểu game state để hỗ trợ `rdeepseq` trong xử lý song song.

## 2. Công Nghệ Sử Dụng
- Ngôn ngữ: Haskell (GHC 9.6.x)
- Build: Stack
- Rendering: `sdl2`, `sdl2-image`, `sdl2-ttf`
- Networking: `websockets`
- Concurrency: `stm`, `forkIO`
- Parallel: `parallel`, `deepseq` (với `NFData` trên các kiểu trong `shared`)
- Serialization/JSON: `aeson`, `binary`

## 3. Tính Năng Hiện Có
- Client–Server chạy được; client kết nối WebSocket, nhận `GameState`, render hoạt ảnh nhân vật/quái.
- AI cơ bản cho quái: tìm người chơi gần nhất, đuổi theo, tấn công trong tầm; có timer death/hit.
- Combat: đòn cận chiến cơ bản, tính sát thương, trạng thái `Hit/Dead`, lives và hẹn giờ respawn.
- Vật lý: cập nhật projectile (song song) và lọc theo lifetime.
- Assets: hoạt ảnh người chơi (Mage_1, Mage_2), quái (Cultist Priest, Frost Ice), UI tiles.

## 4. Cấu Trúc Thư Mục
```
Gamedev/
├── stack.yaml                      # Quản lý workspace (client/server/shared)
├── README.md                       # Tài liệu này
├── client/
│   ├── package.yaml                # Cấu hình & phụ thuộc client
│   ├── client.cabal
│   ├── app/
│   │   └── Main.hs                 # Entry point client (SDL init, loop, network)
│   ├── assets/
│   │   ├── maps/                   # *.json (sample_map.json)
│   │   └── textures/
│   │       ├── mage/               # Mage_1, Mage_2 (idle/run/attack/...)
│   │       ├── monsters/           # Cultist_Priest, FrostIce (idle/walk/atk/...)
│   │       └── ui/                  # tiles, nền, trang trí
│   └── src/
│       ├── Main.hs                 # Khởi tạo, vòng lặp render, input, network
│       ├── Core/
│       │   ├── Animation.hs        # Kiểu Animation, cập nhật frame
│       │   ├── Audio.hs            # (Hiện chưa dùng)
│       │   ├── Input.hs            # Xử lý sự kiện input
│       │   ├── PlayerAssets.hs     # Map key → hoạt ảnh theo mage
│       │   └── Renderer.hs         # Kết xuất frame từ World + Resources
│       ├── Network/
│       │   └── ClientNet.hs        # Vòng lặp WebSocket phía client
│       ├── Renderer/
│       │   └── Resources.hs        # Nạp tất cả sprite frames → ResourceMap
│       ├── Systems/
│       │   └── MapLoader.hs        # Nạp tệp map JSON
│       └── UI/
│           ├── HUD.hs              # (Hiện chưa dùng)
│           └── Menu.hs             # Menu/placeholder
│
├── server/
│   ├── package.yaml                # Cấu hình & phụ thuộc server (websockets, parallel, ...)
│   ├── server.cabal
│   ├── app/
│   │   ├── Main.hs                 # Chạy WebSocket server + spawn game loop
│   │   └── ServerApp.hs            # Server state, game loop, websocket handlers
│   └── src/
│       ├── Systems/
│       │   ├── EntitySystem.hs     # Quản lý World (players, respawn, ...)
│       │   ├── AISystem.hs         # Tiện ích/khung xử lý AI
│       │   ├── CombatSystem.hs     # Tính sát thương, trạng thái combat
│       │   ├── DungeonSystem.hs    # Spawn quái ngẫu nhiên
│       │   ├── PhysicsSystem.hs    # Cập nhật projectile (song song)
│       │   ├── SkillSystem.hs      # Khung xử lý kỹ năng
│       │   ├── ItemSystem.hs       # Khung vật phẩm trong world
│       │   ├── PartySystem.hs      # Khung tổ đội
│       │   ├── EffectSystem.hs     # Khung hiệu ứng/buff
│       │   └── SyncSystem.hs       # Chuẩn bị dữ liệu gửi client
│       └── Core/                   # (Hiện không dùng; có thể dọn nếu trống)
│
├── shared/
│   ├── package.yaml                # Cấu hình & phụ thuộc shared (aeson, deepseq, ...)
│   ├── shared.cabal
│   └── src/
│       ├── Network/
│       │   └── Protocol.hs         # Định nghĩa thông điệp Client/Server
│       └── Types/
│           ├── Common.hs           # Vector2D, Direction, EntityState, toán vector
│           ├── Player.hs           # Player + PlayerMage
│           ├── Enemy.hs            # Enemy + EnemyType + AIState
│           ├── GameTypes.hs        # GameState, Projectile, WorldItem (derive NFData)
│           ├── Item.hs             # Kiểu Item (khung)
│           ├── Account.hs          # Kiểu LoginResponse (khung)
│           ├── NetworkTypes.hs     # Kiểu input/packet hỗ trợ
│           └── Config.hs           # Hằng số gameplay
│
├── vendor/                         # Tệp hỗ trợ build SDL2 trên Windows
└── samples/                        # Mẫu thử tách biệt
```

Ghi chú dọn dẹp (tùy chọn):
- Không sử dụng: `client/src/Core/Audio.hs`, `client/src/UI/HUD.hs`, `server/src/Core/*` → có thể xóa nếu không dùng.

## 5. Thiết Lập & Chạy
Yêu cầu: Stack, GHC phù hợp (resolver trong `stack.yaml`). Trên Windows, đảm bảo SDL2 runtime hiện diện (thư mục `vendor/` đã hỗ trợ cấu hình).

Biên dịch toàn bộ (client + server + shared):
```powershell
stack build
```

Chạy server (WebSocket tại 127.0.0.1:9160):
```powershell
stack exec arcane-server
```

Chạy client (kết nối vào localhost:9160):
```powershell
stack exec arcane-client
```

Mẹo chạy song song (hai cửa sổ PowerShell):
- Cửa sổ 1: `stack exec arcane-server`
- Cửa sổ 2: `stack exec arcane-client`

## 6. Thiết Kế Nổi Bật
- Authoritative server: tránh gian lận, trạng thái nhất quán.
- STM + TQueue: hàng đợi input, broadcast snapshot an toàn khi nhiều client.
- Parallel tick: tách việc cập nhật projectile theo lô để tận dụng đa lõi.
- Shared types: một nguồn sự thật (SSOT) cho kiểu dữ liệu giữa client/server.

## 7. Lộ Trình Mở Rộng
- Bổ sung HUD/Audio, màn hình Login/Lobby nếu cần.
- Thêm va chạm tilemap/obstacle, knockback, kỹ năng tầm xa có projectile assets.
- Cấu hình YAML thực (đọc cổng, tickrate, spawn, …) thay vì hằng số compile‑time.
- Kiểm thử: property test cho serialization (Aeson/Binary) của `GameState` và thông điệp mạng.

---
Nếu gặp lỗi build liên quan `NFData`, đảm bảo `shared` đã thêm `deepseq` và các kiểu trong `Types/*` có `deriving NFData`. Port WebSocket mặc định: `9160`. Bạn có thể chỉnh trong `server/app/Main.hs` nếu cần.
