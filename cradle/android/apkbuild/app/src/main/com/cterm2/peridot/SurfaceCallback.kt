package com.cterm2.peridot

import android.app.Activity
import android.arch.lifecycle.ViewModel
import android.util.Log
import android.view.SurfaceHolder

final class NativeEngine : ViewModel() {
    fun init() {
        Log.v("peridot bootstrap", "surfaceCreated: init nativeEngine")
    }
}

final class SurfaceCallback(private val parent: NativeActivity) : SurfaceHolder.Callback {
    override fun surfaceCreated(p0: SurfaceHolder?) {
    }

    override fun surfaceDestroyed(p0: SurfaceHolder?) {
        Log.v("peridot bootstrap", "Not yet implemented")
    }

    override fun surfaceChanged(holder: SurfaceHolder?, format: Int, width: Int, height: Int) {
        Log.v("peridot bootstrap", "surfaceChanged with $format, $width x $height")
        parent.nativeEngine.init()
    }
}
