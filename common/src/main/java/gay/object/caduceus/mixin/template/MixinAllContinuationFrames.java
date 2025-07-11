package gay.object.caduceus.mixin.template;

import at.petrak.hexcasting.api.casting.eval.vm.*;
import at.petrak.hexcasting.api.casting.iota.Iota;
import at.petrak.hexcasting.api.casting.iota.NullIota;
import com.llamalad7.mixinextras.injector.wrapoperation.Operation;
import com.llamalad7.mixinextras.injector.wrapoperation.WrapOperation;
import gay.object.caduceus.utils.continuation.ContinuationMarkHolder;
import org.jetbrains.annotations.Nullable;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.Pseudo;
import org.spongepowered.asm.mixin.Unique;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

// targets are dynamically added via gay.object.caduceus.mixin.MixinConfigPlugin
// if that fails, this mixin is used as a fallback without modification
@SuppressWarnings("UnusedMixin")
@Pseudo
@Mixin(value = {FrameEvaluate.class, FrameForEach.class}, remap = false)
public abstract class MixinAllContinuationFrames implements ContinuationFrame, ContinuationMarkHolder {
    @Unique
    @Nullable
    private Iota caduceus$mark;

    @Unique
    @Override
    public Iota caduceus$getMark() {
        if (caduceus$mark == null) {
            return new NullIota();
        }
        return caduceus$mark;
    }

    @Unique
    @Override
    public void caduceus$setMark(Iota newMark) {
        caduceus$mark = newMark;
    }

    @WrapOperation(
        method = "evaluate",
        at = @At(value = "INVOKE", target = "Lat/petrak/hexcasting/api/casting/eval/vm/SpellContinuation;pushFrame(Lat/petrak/hexcasting/api/casting/eval/vm/ContinuationFrame;)Lat/petrak/hexcasting/api/casting/eval/vm/SpellContinuation;"),
        require = 0
    )
    private SpellContinuation caduceus$addMarkToPushedFrame(SpellContinuation instance, ContinuationFrame frame, Operation<SpellContinuation> original) {
        // if we're pushing a frame which is the same class as (or a subclass of) this one, copy the mark over
        // this handles cases like FrameEvaluate creating a new frame with list.cdr
        if (getClass().isInstance(frame)) {
            caduceus$setMarkSafe(frame, caduceus$mark);
        }
        return original.call(instance, frame);
    }

    @Inject(method = "copy", at = @At("RETURN"), require = 0)
    private void caduceus$addMarkToCopy(CallbackInfoReturnable<ContinuationFrame> cir) {
        caduceus$setMarkSafe(cir.getReturnValue(), caduceus$mark);
    }

    @Unique
    private void caduceus$setMarkSafe(ContinuationFrame other, Iota newMark) {
        // check for singletons
        if (other != this && other instanceof ContinuationMarkHolder holder) {
            holder.caduceus$setMark(newMark);
        }
    }
}
