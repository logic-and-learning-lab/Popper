#T1
my_true_score(white,60).
my_true_control(white).
does(white,lay_claim).
my_true_score(black,60).
my_true_claim_made_by(black).
does(black,noop).


#T10
my_true_control(white).
does(white,lay_claim).
my_true_score(black,65).
my_true_score(white,65).
my_true_claim_made_by(black).
does(black,noop).

#T11
my_true_control(white).
my_true_score(black,70).
my_true_claim_made_by(black).
does(black,noop).
does(white,end_game).
my_true_score(white,70).

#T12
my_true_control(black).
does(black,lay_claim).
does(white,noop).
my_true_claim_made_by(white).
my_true_score(white,70).
my_true_score(black,75).

#T13
my_true_control(black).
my_true_score(black,70).
does(black,end_game).
my_true_score(white,65).
does(white,noop).
my_true_claim_made_by(white).

#T2
my_true_control(white).
does(white,lay_claim).
my_true_score(black,70).
my_true_claim_made_by(black).
does(black,noop).
my_true_score(white,70).

#T4
my_true_score(white,55).
my_true_score(black,60).
my_true_control(black).
does(black,end_game).
does(white,noop).
my_true_claim_made_by(white).

#T5
my_true_score(black,80).
my_true_control(black).
does(black,lay_claim).
does(white,noop).
my_true_claim_made_by(white).
my_true_score(white,75).

#T6
my_true_control(white).
my_true_score(black,65).
my_true_score(white,65).
my_true_claim_made_by(black).
does(black,noop).
does(white,end_game).

#T8
my_true_control(white).
my_true_score(black,80).
does(black,noop).
does(white,end_game).
my_true_score(white,80).

#T9
my_true_control(white).
does(white,lay_claim).
my_true_claim_made_by(black).
does(black,noop).
my_true_score(white,75).
my_true_score(black,75).

#T14
my_true_control(white).
my_true_score(white,95).
my_true_score(black,75).
my_true_gameOver(black).
my_true_gameOver(white).


#T15
my_true_score(white,65).
my_true_score(black,90).
my_true_control(black).
my_true_gameOver(black).
my_true_gameOver(white).

#T16
my_true_control(white).
my_true_score(white,80).
my_true_score(black,60).
my_true_gameOver(black).
my_true_gameOver(white).

#T3
my_true_score(black,70).
my_true_control(white).
my_true_score(white,90).
my_true_gameOver(black).
my_true_gameOver(white).

#T7
my_true_score(black,100).
my_true_score(white,75).
my_true_control(black).
my_true_gameOver(black).