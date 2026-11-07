package io.ct2.peridot;

import android.content.res.AssetManager;
import android.view.Surface;

import java.nio.ByteBuffer;

public class NativeLibLink {
    public static native ByteBuffer init(Surface surface, AssetManager assetManager);
    public static native void fin(ByteBuffer ptr);
    public static native void update(ByteBuffer ptr);

    public static native void processTouchDownEvent(ByteBuffer ptr, int id);
    public static native void processTouchUpEvent(ByteBuffer ptr, int id);
    public static native void setTouchPositionAbsolute(ByteBuffer ptr, int id, float x, float y);

    static {
        System.loadLibrary("pegamelib");
    }
}
