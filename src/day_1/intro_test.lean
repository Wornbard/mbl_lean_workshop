import tactic
--If the above returns an error, you probably didn't install mathlib correctly. Consult the instruction in README carefully
--If you think you did everything correctly, consult me (Jakub Wornbard) on Facebook or however else you like



--You should see the `Lean Infoview` window on the right of the screen. 
--If you don't, you may try `ctrl+shift+enter` 
--If you still don't see it, something is probably wrong. Make sure you installed the Lean extension in VS Code

#eval 2+2
--If you put the cursor at the end of the line above, you should see the result in the Infoview

variable (P : Prop)

example : P→P :=
begin
  --the state of the Infoview should change as you move the cursor down the next few lines.
  --after the last comma, it should say `goals accompished`
  intro hP,
  assumption,
end

--If all of that works you're good to go

