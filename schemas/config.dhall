
let ScreenStyle = < Windowed | Unbordered | Fullscreen >
let Screen = { width : Integer, height : Integer, style : ScreenStyle }

let App = {
	, Type = { screen : Screen }
	, default = {=}
	}

in { App, Screen, ScreenStyle }
