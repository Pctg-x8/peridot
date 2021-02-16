package jp.ct2.peridot

import android.app.Activity
import android.arch.lifecycle.ViewModel
import android.content.res.AssetManager
import android.util.Log
import android.view.Surface
import android.view.SurfaceHolder
import java.nio.ByteBuffer

final class NativeLibLink {
    external fun init(surface: Surface, am: AssetManager): ByteBuffer
    external fun fin(o: ByteBuffer)
    external fun update(o: ByteBuffer)

    external fun processTouchDownEvent(o: ByteBuffer, id: Int)
    external fun processTouchUpEvent(o: ByteBuffer, id: Int)
    external fun setTouchPositionAbsolute(o: ByteBuffer, id: Int, x: Float, y: Float)

    init {
        System.loadLibrary("ntv")
    }
}

final class NativeEngine : ViewModel() {
    private val ntvlink = NativeLibLink()
    var internalPtr: ByteBuffer? = null

    fun init(surface: Surface, am: AssetManager) {
        Log.v("peridot bootstrap", "surfaceCreated: init nativeEngine")
        this.internalPtr = ntvlink.init(surface, am)
    }

    fun fin() {
        this.internalPtr?.let {
            Log.v("peridot bootstrap", "finalizing nativeEngine")
            ntvlink.fin(it)
        }
        this.internalPtr = null
    }

    fun update() {
        this.internalPtr?.let { this.ntvlink.update(it) }
    }

    fun setTouchPositionAbsolute(id: Int, x: Float, y: Float) {
        this.internalPtr?.let { this.ntvlink.setTouchPositionAbsolute(it, id, x, y) }
    }
    fun touchDown(id: Int, x: Float, y: Float) {
        this.internalPtr?.let {
            this.ntvlink.setTouchPositionAbsolute(it, id, x, y)
            this.ntvlink.processTouchDownEvent(it, id)
        }
    }
    fun touchUp(id: Int, x: Float, y: Float) {
        this.internalPtr?.let {
            this.ntvlink.setTouchPositionAbsolute(it, id, x, y)
            this.ntvlink.processTouchUpEvent(it, id)
        }
    }
}

final class SurfaceCallback(private val parent: NativeActivity) : SurfaceHolder.Callback {
    override fun surfaceCreated(p0: SurfaceHolder?) {
    }

    override fun surfaceDestroyed(p0: SurfaceHolder?) {
        Log.v("peridot bootstrap", "Not yet implemented")
        parent.nativeEngine.fin()
    }

    override fun surfaceChanged(holder: SurfaceHolder?, format: Int, width: Int, height: Int) {
        Log.v("peridot bootstrap", "surfaceChanged with $format, $width x $height")
        parent.nativeEngine.init(holder!!.surface, parent.assets)
    }
}
