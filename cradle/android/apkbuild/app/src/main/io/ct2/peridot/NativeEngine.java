package io.ct2.peridot;

import android.content.res.AssetManager;
import android.util.Log;
import android.view.Surface;

import java.nio.ByteBuffer;

import androidx.lifecycle.ViewModel;

public final class NativeEngine extends ViewModel {
    private ByteBuffer internalPtr = null;

    public void init(Surface surface, AssetManager assetManager) {
        Log.v("peridot bootstrap", "init NativeEngine");

        this.internalPtr = NativeLibLink.init(surface, assetManager);
    }

    public void fin() {
        Log.v("peridot bootstrap", "finalizing NativeEngine");

        NativeLibLink.fin(this.internalPtr);
        this.internalPtr = null;
    }

    public void update() {
        NativeLibLink.update(this.internalPtr);
    }

    public void setTouchPositionAbsolute(int id, float x, float y) {
        NativeLibLink.setTouchPositionAbsolute(this.internalPtr, id, x, y);
    }

    public void touchDown(int id, float x, float y) {
        NativeLibLink.setTouchPositionAbsolute(this.internalPtr, id, x, y);
        NativeLibLink.processTouchDownEvent(this.internalPtr, id);
    }

    public void touchUp(int id, float x, float y) {
        NativeLibLink.setTouchPositionAbsolute(this.internalPtr, id, x, y);
        NativeLibLink.processTouchUpEvent(this.internalPtr, id);
    }
}
