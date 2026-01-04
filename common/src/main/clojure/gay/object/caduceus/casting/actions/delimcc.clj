(ns gay.object.caduceus.casting.actions.delimcc
  (:require [gay.object.caduceus.casting.eval.vm.frames :as frames]
            [gay.object.caduceus.utils.casting :as casting]
            [gay.object.caduceus.utils.continuation :as continuation]
            [gay.object.caduceus.casting.mishaps.no-prompt :as no-prompt]
            [gay.object.caduceus.casting.iota.delimcc :as delimcc])
  (:import (at.petrak.hexcasting.api.casting.castables Action)
           (at.petrak.hexcasting.api.casting.iota Iota NullIota)
           (at.petrak.hexcasting.api.casting.mishaps MishapNotEnoughArgs)
           (at.petrak.hexcasting.common.casting.actions.eval OpEval)
           (gay.object.caduceus.casting.eval.vm.frames PromptFrame)
           (java.util ArrayList Collection)))

; from alwinfy:
;   prompt: eval/wrapping, takes a list
;   control: like iris, but
;   (1) only captures the frames up to the last prompt in the call stack
;   (2) ejects those frames as if an exception had been thrown up to the prompt
;   (3) pushes the slice taken in (1), then invokes the code argument
;   invoking those captured frames pushes them to the stack again

(deftype OpPrompt []
  Action
  (operate [_this env image cont]
    (.operate OpEval/INSTANCE
              env
              image
              (.pushFrame cont (frames/empty-prompt-frame)))))

(deftype OpPromptAt []
  Action
  (operate [_this env image cont]
    (let [stack (-> image .getStack vec)
          stack-size (count stack)
          mark (peek stack)]
      (if (< stack-size 2)
        (throw (MishapNotEnoughArgs/new 2 stack-size)))
      (continuation/assert-valid-mark mark 0)
      (.operate OpEval/INSTANCE
          env
          (casting/copy-image
            image
            :stack (pop stack))
          (.pushFrame cont (frames/->PromptFrame mark))))))

(defn split-at-prompt
  ([cont mark] (split-at-prompt '() cont mark))
  ([coll cont mark]
   (if (or (continuation/done? cont)
           (and (instance? PromptFrame (.getFrame cont))
                (Iota/tolerates mark (continuation/get-mark cont))))
     [(continuation/push-all coll) cont]
     (recur
       (conj coll (.getFrame cont))
       (.getNext cont)
       mark))))

(deftype OpControl []
  Action
  (operate [_this env image cont]
    (let [stack (-> image .getStack vec)
          [slice new-cont] (split-at-prompt cont (NullIota/new))]
      (if (empty? stack)
        (throw (MishapNotEnoughArgs/new 1 0)))
      (if (continuation/done? new-cont)
        (throw (no-prompt/->MishapNoPrompt)))
      (.exec OpEval/INSTANCE
             env
             image
             new-cont
             (-> stack
                 pop
                 (conj (delimcc/->DelimitedContinuationIota slice))
                 ^[Collection] ArrayList/new)
             (peek stack)))))

(deftype OpControlAt []
  Action
  (operate [_this env image cont]
    (let [stack (-> image .getStack vec)
          stack-size (count stack)]
      (if (< stack-size 2)
        (throw (MishapNotEnoughArgs/new 2 stack-size)))
      (let [mark (peek stack)
            rest (pop stack)
            [slice new-cont] (split-at-prompt cont mark)]
        (if (continuation/done? new-cont)
          (throw (no-prompt/->MishapNoPrompt)))
        (.exec OpEval/INSTANCE
               env
               image
               new-cont
               (-> rest
                   pop
                   (conj (delimcc/->DelimitedContinuationIota slice))
                   ^[Collection] ArrayList/new)
               (peek rest))))))
