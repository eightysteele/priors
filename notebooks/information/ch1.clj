^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns information.ch1
  {:nextjournal.clerk/toc true}
  (:refer-clojure
   :exclude [+ - * / zero? compare divide numerator denominator
             infinite? abs ref partial =])
  (:require [emmy.clerk :as ec]
            [emmy.differential]
            [emmy.env :as emmy]
            [nextjournal.clerk :as clerk]
            [clojure.math :as math]
            [clojure.repl :as repl]
            [gen.distribution.commons-math :as dist]
            [gen.dynamic :as dynamic :refer [gen]]
            [gen.choicemap :as choicemap]
            [gen.generative-function :as gf]
            [gen.trace :as trace]))

^{::clerk/visibility {:code :hide :result :hide}}
(ec/install!)

;; # Chapter 1. Information as Choice

;; ## Overview

;; This chapter defines the Nyquist-Hartley definition of information. At the
;; end it shows how to compute information using probabilities, assuming a
;; uniform distribution of symbols.

;; ## History and motivation

;; In the 1920s, Harry Nyquist and Ralph Hartley set out to improve telegraphic
;; transmission speeds at AT&T Bell Labs. To study the problem, they invented a
;; measure of information. A message **symbol** was the smallest particle of
;; **information**. In theory, it was anything discrete that could be perceived.
;; A **message** was then just an ordered set of symbols, serving as the carrier
;; of information.

;; ## How much information is in a message?

;; How much information a message contains depends on if you're the sender—or
;; the receiver—of the message. Suppose we have two symbols (a black ball and a
;; white ball) with a message that carries exactly one symbol. If the sender is
;; free to choose either ball independently, how much information does a message
;; carry? Nyquist and Hartley posited the amount of information is just the
;; number of different messages $N$ from which it was chosen,

;; $$
;; \begin{equation}
;; N = S^n
;; \end{equation}
;; $$

(defn number-of-messages
  [S n]
  (emmy/expt S n))

;; where $S$ is the number of symbol values and $n$ is the number of symbols in
;; a message. By this measure, the information in a message is $N$,

;; $$
;; \begin{equation}
;; I_{Message}^b = N
;; \end{equation}
;; $$

;; where $b$ means information **before** the message is sent. In our example
;; above, $S$ is 2 (black or white) and $n$ is 1. Since $2^1 = 2$ there are two
;; different messages available—two choices. For the sender, information is
;; literally a matter of counting choices.

;; ## Counting choices logarithmically

;; In (2), notice how information grows exponentially in terms of message length
;; $n$. Nyquist and Hartley thought that was weird. In terms of transmission
;; capacity, adding one symbol to a message certaintly did't require _that_ much
;; extra capacity. To make information linearly proportional to $n$, they just
;; took the base 2 logarithm of $N$, written as

;; $$
;; \begin{equation}
;; I_{Message}^b = log_2 N
;; \end{equation}
;; $$

(defn nyquist-hartley-information
  [N]
  (emmy/log2 N))

;; By taking the logarithm, they were now counting the **relative** number of
;; message choices. The absolute number of choices could still be computed by
;; reversing the logarithm

;; $$
;; N = 2^{I_{Message}^b}
;; $$

;; Unsurprisingly, the same approach they took for a message worked for a
;; symbol,

;; $$
;; I_{Symbol}^b = log_2 S
;; $$

;; where $S$, as before, is the number of different symbol values on offer.
;; Also, since logarithms transform multiplication into addition, they could
;; compute the information in a sent message by summing the information in each
;; symbol it carried,

;; $$
;; I_{Message}^b = \displaystyle\sum_{i=1}^n I_{Symbol\_i}^b
;; $$

;; where $n$, as before, is the message length.

;; ### Example computations

;; Here's a function to compute the Nyquist-Hartley definition of information,

(defn compute-nyquist-hartley-information
  [S n]
  (let [N (number-of-messages S n)]
    (nyquist-hartley-information N)))

;; Going back to our black and white ball example, how many bits of information
;; is in a single message that carries one symbol?

^{::clerk/visibility {:result :show}}
(compute-nyquist-hartley-information 2 1)

;; How many bits of information is in a single message that carries one
;; character of the English alphabet?

^{::clerk/visibility {:result :show}}
(compute-nyquist-hartley-information 26 1)


;; How many bits if we send five characters instead of one?

^{::clerk/visibility {:result :show}}
(compute-nyquist-hartley-information 26 5)

;; ## Probabilistically computing information

;; Another way to count possibilities is to compute probabilities.
;; Nyquist-Hartley assumed a uniform distribution of symbols, which was limited,
;; since some symbols are more likely than others (for example, consider how
;; common the letter "e" is compared to "z" is in the English alphabet). In
;; chapter 2, Shannon drops information entropy which measures the **average
;; information** per message (or symbol!) using linear combinations of symbol
;; probabilities. In preparation for that, let's compute some probabilities.

;; By taking the probability of any message,

;; $$
;; P_{Message}^b = \cfrac{1}{N}
;; $$

;; we can then get the **count** of equiprobable messages,

;; $$
;; {N} = \cfrac{1}{P_{Message}^b}
;; $$

;; and substituting $N$ into (3) gives us information for each message, based on
;; its probability, under a uniform distribution of an endless supply of
;; symbols

;; $$
;; \begin{equation}
;; I_{Message}^b = \log_2 N = \log_2 \cfrac{1}{P_{Message}^b} = - \log_2 {P_{Message}^b}
;; \end{equation}
;; $$


(defn uniform-probability-information
  [P]
  (emmy/- (emmy/log2 P)))

;; As with logarithms, the same approach that works for messages can be applied
;; to symbols

;; $$
;; P_{Symbol}^b = \cfrac{1}{S}
;; $$

;; $$
;; S = \cfrac{1}{P_{Symbol}^b} 
;; $$

;; $$
;; \therefore I_{Symbol}^b = \log_2 S = \log_2 \cfrac{1}{P_{Symbol}^b} = - \log_2 {P_{Symbol}^b}
;; $$

;; which means that for a message with $n$ symbols, the information it carries
;; can be probabalistically computed by summing up symbol probabilities

;; $$
;; I_{Message}^b = - \displaystyle\sum_{i=1}^n P_{Symbol\_i}^b
;; $$

;; ### Spot check

;; We would expect that computating information with the logarithmic approach
;; would produce the same results as computing information using probabilities.
;; Let's check!

(defn compute-uniform-information
  [S n]
  (let [N (number-of-messages S n)
        P (emmy/divide 1.0 N)]
    (uniform-probability-information P)))

(emmy/=
 (compute-uniform-information 2 1) (compute-nyquist-hartley-information 2 1))

(emmy/=
 (compute-uniform-information 26 1) (compute-nyquist-hartley-information 26 1))

(emmy/=
 (compute-uniform-information 26 5) (compute-nyquist-hartley-information 26 5))

;; ## Recap

;; Nyquist and Hartley quantified information in a message—before it was sent—as
;; the count of different messages from which it was chosen. To make information
;; linearly proportional to message length, they went with relative counts using
;; logarithms. The main assumption in their approach was an independent
;; selection of an endless supply of symbols from a uniform distribution. We
;; ended by exploring an approach to computing information based on
;; probabilities.

;; ## Next up

;; In chapter 2 the author explores computing information based on a non-uniform
;; distribution of symbols using Shannon's method of computing the average
;; information per message.
