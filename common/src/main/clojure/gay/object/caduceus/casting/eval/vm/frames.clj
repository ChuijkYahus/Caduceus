(ns gay.object.caduceus.casting.eval.vm.frames
  (:import (at.petrak.hexcasting.api.casting.eval CastResult ResolvedPatternType)
           (at.petrak.hexcasting.api.casting.eval.vm ContinuationFrame ContinuationFrame$Type)
           (at.petrak.hexcasting.api.casting.iota Iota IotaType NullIota)
           (at.petrak.hexcasting.common.lib.hex HexEvalSounds)
           (gay.object.caduceus.utils.continuation ContinuationMarkHolder)
           (kotlin Pair)))

(declare prompt-frame-type)

; A stack marker (like FrameFinishEval) for delimited continuations.
(deftype PromptFrame [^Iota ^:unsynchronized-mutable mark]
  ContinuationFrame
  (breakDownwards [_this stack] (Pair/new false stack))
  (evaluate [_this cont _level _harness]
        (CastResult/new
              (NullIota/new)
              cont
              nil
              []
              ResolvedPatternType/EVALUATED
              HexEvalSounds/NOTHING))
  (serializeToNBT [_this]
    (let [tag (net.minecraft.nbt.CompoundTag/new)]
      (.put tag "mark" (IotaType/serialize mark))
      tag))
  (size [_this] (.size mark))
  (getType [_this] prompt-frame-type)
  ContinuationMarkHolder
  (caduceus$getMark [_this] mark)
  (caduceus$setMark [_this new-mark] (set! mark new-mark)))

(defn empty-prompt-frame []
  (->PromptFrame (NullIota/new)))

(def prompt-frame-type
  (reify
    ContinuationFrame$Type
    (deserializeFromNBT [_this tag world]
      (if (.contains tag "mark")
        (-> tag
            (.getCompound "mark")
            (IotaType/deserialize world)
            ->PromptFrame)
        (empty-prompt-frame)))))
