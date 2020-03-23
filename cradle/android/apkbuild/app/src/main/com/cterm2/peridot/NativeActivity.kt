// Peridot Bootstrap Activity

package com.cterm2.peridot

import android.arch.lifecycle.ViewModelProvider
import android.os.Build
import android.support.v7.app.AppCompatActivity
import android.os.Bundle
import android.view.SurfaceView
import android.view.View

class NativeActivity : AppCompatActivity() {
    val nativeEngine: NativeEngine by lazy {
        val factory = ViewModelProvider.NewInstanceFactory()
        ViewModelProvider(this, factory).get(NativeEngine::class.java)
    }

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)

        this.hideDecorationUIs()

        val surface = SurfaceView(this)
        surface.holder.addCallback(SurfaceCallback(this))
        setContentView(surface)
    }

    override fun onResume() {
        super.onResume()
        this.hideDecorationUIs()
    }

    private fun hideDecorationUIs() {
        if (Build.VERSION.SDK_INT >= 19) {
            this.window.decorView.systemUiVisibility = View.SYSTEM_UI_FLAG_HIDE_NAVIGATION or
                    View.SYSTEM_UI_FLAG_LAYOUT_HIDE_NAVIGATION or
                    View.SYSTEM_UI_FLAG_LAYOUT_FULLSCREEN or
                    View.SYSTEM_UI_FLAG_FULLSCREEN or
                    View.SYSTEM_UI_FLAG_IMMERSIVE_STICKY
        }
    }
}
