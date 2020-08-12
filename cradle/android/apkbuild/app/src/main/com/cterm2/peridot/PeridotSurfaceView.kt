package com.cterm2.peridot

import android.content.Context
import android.util.Log
import android.view.MotionEvent
import android.view.SurfaceView

class PeridotSurfaceView(parent: Context) : SurfaceView(parent) {
    override fun onTouchEvent(event: MotionEvent?): Boolean {
        return event?.let {
            when (it.actionMasked) {
                MotionEvent.ACTION_DOWN -> {
                    for (i in 0 until it.pointerCount) {
                        val pid = it.getPointerId(i)
                        val x = it.getX(i)
                        val y = it.getY(i)
                        Log.d("PeridotRenderView", "TouchDown! x=${x} y=${y} idx=${pid}")
                    }
                    true
                }
                MotionEvent.ACTION_MOVE -> {
                    for (i in 0 until it.pointerCount) {
                        val pid = it.getPointerId(i)
                        val x = it.getX(i)
                        val y = it.getY(i)
                        Log.d("PeridotRenderView", "TouchMove! x=${x} y=${y} idx=${pid}")
                    }
                    true
                }
                MotionEvent.ACTION_UP -> {
                    for (i in 0 until it.pointerCount) {
                        val pid = it.getPointerId(i)
                        val x = it.getX(i)
                        val y = it.getY(i)
                        Log.d("PeridotRenderView", "TouchUp! x=${x} y=${y} idx=${pid}")
                    }
                    true
                }
                MotionEvent.ACTION_CANCEL -> {
                    for (i in 0 until it.pointerCount) {
                        val pid = it.getPointerId(i)
                        val x = it.getX(i)
                        val y = it.getY(i)
                        Log.d("PeridotRenderView", "TouchCancel! x=${x} y=${y} idx=${pid}")
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