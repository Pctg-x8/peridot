
let ScreenStyle = < Windowed | Unbordered | Fullscreen >
let Screen = {
	, Type = { width : Natural, height : Natural, style : ScreenStyle }
	, default = { style = ScreenStyle.Windowed }
	}

let App = {
	, Type = { screen : Screen.Type }
	, default = { screen = Screen.default }
	}

in { App, Screen, ScreenStyle }
