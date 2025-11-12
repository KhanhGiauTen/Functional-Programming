module UI.HUD where

import Graphics.Gloss
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

-- Import các kiểu dữ liệu
import Renderer.Resources (ResourceMap)
import Types.GameTypes (Player(..))
import Core.Animation (Animation(..))

-- | Hàm chính để vẽ HUD (Máu, Mana)
drawHUD :: ResourceMap -> Player -> Picture
drawHUD res player =
	let
		-- Lấy ảnh từ ResourceMap
		-- (Dùng 'getFrame' để lấy ảnh đầu tiên của animation)
		healthFrame = getFrame res "ui_health_frame"
		healthFill  = getFrame res "ui_health_fill"
		manaFill    = getFrame res "ui_mana_fill"

		-- Tính toán tỷ lệ %
		healthPercent = (fromIntegral $ playerHealth player) / (fromIntegral $ maxHealth player)
		manaPercent   = (fromIntegral $ playerMana player) / (fromIntegral $ maxMana player)

		-- Co giãn (Scale) thanh máu/mana
		-- (Giả sử ảnh gốc rộng 100px, cao 20px)
		-- Chúng ta chỉ co giãn theo chiều X
		scaledHealth = Scale healthPercent 1 healthFill
		scaledMana   = Scale manaPercent 1 manaFill

		-- Đặt thanh máu ở góc dưới trái (ví dụ: tọa độ -350, -280)
		healthBar = Translate (-350) (-280) $ Pictures
			[ healthFrame
			, scaledHealth
			]
    
		-- Đặt thanh mana ngay dưới thanh máu
		manaBar = Translate (-350) (-260) $ Pictures
			[ healthFrame -- (Dùng chung 1 frame)
			, scaledMana
			]

	in
		Pictures [healthBar, manaBar]


-- | Hàm helper để lấy frame đầu tiên (Picture) từ 1 Animation
getFrame :: ResourceMap -> String -> Picture
getFrame res key =
	case Map.lookup key res of
		Nothing -> Blank -- Trả về ảnh trống nếu không tìm thấy
		Just anim ->
			case animFrames anim of
				[] -> Blank -- Trả về ảnh trống nếu animation rỗng
				(firstFrame:_) -> firstFrame -- Lấy frame đầu tiên