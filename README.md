Project Description. In this project you are going to implement a Prolog program to
assign TAs to proctor quizzes based on their teaching schedule. There are quizzes at
different timings during the week and each quiz needs a specific number of proctors.
There might also be multiple quizzes during the same slot. These proctors are TAs but
they cannot be assigned to a quiz on a slot that they also teach on. They also cannot
be assigned to proctor a quiz on their day off. Therefore, we need a system to find any
possible assignments of TAs to quizzes, verify a given assignment or output false if no
assignment is possible.
Below are the main constraints that need to be satisfied any proctoring assignment:
-Each quiz needs a given number of proctors.
-No TA can be assigned to proctor at the same time as a teaching slot.
-No TA can be assigned to proctor on their day-off.
-A TA cannot be assigned to two quizzes during the same slot
