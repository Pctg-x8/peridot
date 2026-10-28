package io.ct2.peridot;

import android.os.Bundle;
import android.util.Log;
import android.view.Choreographer;
import android.view.MotionEvent;
import android.view.SurfaceHolder;
import android.view.SurfaceView;
import android.view.View;

import androidx.annotation.NonNull;
import androidx.appcompat.app.AppCompatActivity;
import androidx.lifecycle.ViewModelProvider;
import androidx.annotation.Nullable;

public class PeridotActivity extends AppCompatActivity implements Choreographer.FrameCallback {
    private NativeEngine nativeEngine = null;

    @Override
    protected void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        nativeEngine = new ViewModelProvider(this).get(NativeEngine.class);

        this.hideDecorationUIs();
        final var surface = new SurfaceView(this) {
            @Override
            public boolean onTouchEvent(MotionEvent event) {
                if (!PeridotActivity.this.tryProcessTouchEvent(event)) {
                    return super.onTouchEvent(event);
                }

                return true;
            }
        };
        surface.getHolder().addCallback(new SurfaceHolder.Callback() {
            @Override
            public void surfaceCreated(@NonNull SurfaceHolder surfaceHolder) {
                Log.v("peridot bootstrap", "surfaceCreated");
            }

            @Override
            public void surfaceDestroyed(@NonNull SurfaceHolder surfaceHolder) {
                Log.v("peridot bootstrap", "surfaceDestroyed");

                Choreographer.getInstance().removeFrameCallback(PeridotActivity.this);
                PeridotActivity.this.nativeEngine.fin();
            }

            @Override
            public void surfaceChanged(@NonNull SurfaceHolder surfaceHolder, int format, int width, int height) {
                Log.v("peridot bootstrap", "surfaceChanged with " + format + ", " + width + " x " + height);

                PeridotActivity.this.nativeEngine.init(surfaceHolder.getSurface(), PeridotActivity.this.getAssets());
                Choreographer.getInstance().postFrameCallback(PeridotActivity.this);
            }
        });
        this.setContentView(surface);
    }

    @Override
    protected void onResume() {
        super.onResume();

        this.hideDecorationUIs();
    }

    private boolean tryProcessTouchEvent(MotionEvent event) {
        return switch (event.getActionMasked()) {
            case MotionEvent.ACTION_DOWN -> {
                for (var i = 0; i < event.getPointerCount(); i++) {
                    this.nativeEngine.touchDown(
                            event.getPointerId(i),
                            event.getX(i),
                            event.getY(i));
                }

                yield true;
            }
            case MotionEvent.ACTION_UP -> {
                for (var i = 0; i < event.getPointerCount(); i++) {
                    this.nativeEngine.touchUp(
                            event.getPointerId(i),
                            event.getX(i),
                            event.getY(i));
                }

                yield true;
            }
            case MotionEvent.ACTION_MOVE -> {
                for (var i = 0; i < event.getPointerCount(); i++) {
                    this.nativeEngine.setTouchPositionAbsolute(
                            event.getPointerId(i),
                            event.getX(i),
                            event.getY(i));
                }

                yield true;
            }
            case MotionEvent.ACTION_CANCEL -> {
                // Treat as TouchUp
                // https://developer.android.com/reference/android/view/MotionEvent#ACTION_CANCEL
                for (var i = 0; i < event.getPointerCount(); i++) {
                    this.nativeEngine.touchUp(
                            event.getPointerId(i),
                            event.getX(i),
                            event.getY(i));
                }

                yield true;
            }
            case MotionEvent.ACTION_OUTSIDE -> {
                Log.d("PeridotSurfaceView", "TouchOutside!");
                yield true;
            }
            default -> false;
        };
    }

    private void hideDecorationUIs() {
        this.getWindow().getDecorView().setSystemUiVisibility(
                View.SYSTEM_UI_FLAG_HIDE_NAVIGATION |
                        View.SYSTEM_UI_FLAG_LAYOUT_HIDE_NAVIGATION |
                        View.SYSTEM_UI_FLAG_LAYOUT_FULLSCREEN |
                        View.SYSTEM_UI_FLAG_FULLSCREEN |
                        View.SYSTEM_UI_FLAG_IMMERSIVE_STICKY);
    }

    @Override
    public void doFrame(long _frameTimeNanos) {
        this.nativeEngine.update();
        Choreographer.getInstance().postFrameCallback(this);
    }
}
