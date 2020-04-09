## todo: 


Functionality: 
[ ] feature: comments syntax
[ ] Load sample code (while still not creating a new buffer)
[x] Bug: accept whitespace at beginning of input
[ ] Bug: 
Input:
(= r0 [-0.08843 ± 0.00524])
(= tau (/ 1 r0))
Output: 
[-0.089 ± 0.005] (NaN%)
[NaN ± NaN] (5.0%)


[x] bug: "create New" does everythign right, expce=t that the textarea contents are still the old ones. TextArea.innerHTML is correct.
    - happens: create new. type. switch.
    - happens: <start> type. switch. 
    - doesn't happen: <start> load any file. switch any file. 

    - happens even when halogen updates are turned off. 
    - Solution: Set the textarea contents with .value, rather than innerHTML


Design: 
[ ] syntax highlighting
[ ] show results with scientific notation
[ ] align digits of results
[ ] if variable values are shown in a separate section, show them with variable names
[ ] use aesthetic words for random file names. example: https://www.buzzfeed.com/danieldalton/bob-ombinate

Technical:
-- Project: Do I need to keep local state, or should I lift it? 


Other:
[x] provide a way to take comments (netlify forms)
[ ] documentation. 
[ ] how to use with (Xe)LaTeX 


## Other notes

Halogen 5, which is currently on pre-release

https://en.wikipedia.org/wiki/Propagation_of_uncertainty#Ratios
