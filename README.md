# How to use

You should be able to see and interact with all the files that I plan on using in the workshop, and some that I probably will not.

A tentative plan of the classes is [here](/src/plan.md). 

Most of the important content is written in comments in the files that we will be working with.
This means that if :

- you are bored during the class
- don't want to/can't attend

you should be able to learn something just by reading the files and trying to solve them on your own. I will also upload solutions at some point.

Some things may be relegated to the [notes](/src/notes.md) instead.
There is probably no point in looking at them before the first class but afterwards they may be a useful reference.

Finally, if you have a lot of time on your hands, the contents of most of the files in the workshop are HEAVILY inspired by another workshop held by prof. Kevin Buzzard.
In fact it's a rather small subset of that workshop.
You can find it [here](https://xenaproject.wordpress.com/2021/01/21/formalising-mathematics-an-introduction/).
For anyone interested, I strongly recommend checking out the whole website and working through prof. Buzzard's workshop.

# How to install

## The preferred method

1. Follow [this](https://leanprover-community.github.io/get_started.html) to install Lean 3 and all other necessary tools

2. Create a directory where you want to store the contents of the repo and navigate to it in the command line

3. Run
```
leanproject get Wornbard/mbl_lean_workshop

code mbl_lean_workshop
```

4. Navigate to `src/day1/intro_test.lean` and follow the instructions there to see if Lean is working correctly

## The "no commitment" method

You can also try using the [community web editor](https://leanprover-community.github.io/lean-web-editor). It is very slow in my experience but should get the job done.

To use `Load .lean from URL`, go to the github repo, open a file you want to run, click on the `raw` button and paste the URL that it redirects you to. Note it will not work on the last day (in its current version) because `subgroups.lean` imports `groups.lean`. If it becomes necessary to resolve it, I will think of something.

