package jp.ct2.peridot

import android.content.Context
import android.util.Log
import android.view.MotionEvent
import android.view.SurfaceView

class PeridotSurfaceView(private val parent: NativeActivity) : SurfaceView(parent) {
    override fun onTouchEvent(event: MotionEvent?): Boolean {
        return event?.let {
            when (it.actionMasked) {
                MotionEvent.ACTION_DOWN -> {
                    for (i in 0 until it.pointerCount) {
                        val pid = it.getPointerId(i)
                        val x = it.getX(i)
                        val y = it.getY(i)
                        Log.d("PeridotRenderView", "TouchDown! x=${x} y=${y} idx=${pid}")
                        parent.nativeEngine.touchDown(pid, x, y)
                    }
                    true
                }
                MotionEvent.ACTION_MOVE -> {
                    for (i in 0 until it.pointerCount) {
                        val pid = it.getPointerId(i)
                        val x = it.getX(i)
                        val y = it.getY(i)
                        Log.d("PeridotRenderView", "TouchMove! x=${x} y=${y} idx=${pid}")
                        parent.nativeEngine.setTouchPositionAbsolute(pid, x, y)
                    }
                    true
                }
                MotionEvent.ACTION_UP -> {
                    for (i in 0 until it.pointerCount) {
                        val pid = it.getPointerId(i)
                        val x = it.getX(i)
                        val y = it.getY(i)
                        Log.d("PeridotRenderView", "TouchUp! x=${x} y=${y} idx=${pid}")
                        parent.nativeEngine.touchUp(pid, x, y)
                    }
                    true
                }
                MotionEvent.ACTION_CANCEL -> {
                    for (i in 0 until it.pointerCount) {
                        val pid = it.getPointerId(i)
                        val x = it.getX(i)
                        val y = it.getY(i)
                        Log.d("PeridotRenderView", "TouchCancel! x=${x} y=${y} idx=${pid}")
                        // Treat as TouchUp
                        // https://developer.android.com/reference/android/view/MotionEvent#ACTION_CANCEL
                        parent.nativeEngine.touchUp(pid, x, y)
                    }
                    true
                }
                MotionEvent.ACTION_OUTSIDE -> {
                    Log.d("PeridotRenderView", "TouchOutside!")
                    true
                }
                else -> super.onTouchEvent(event)
            }
        } ?: super.onTouchEvent(event)
    }
}
