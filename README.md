# Hybrid recommendation filter
This hybrid recommendation filter combines text mining content-filtering and collaborative filtering to predict the top-N documents that are of most interest for each user. The content-based filter clusters notifications to find content with topics for which the user has shown interest. The collaborative filter increases diversity by discovering new topics of interest for the user, because these are of  interest to other users with similar concerns.

You can find a detailed description and evaluation of this filter in this paper:

<cite>López Hernández, Fernando, et al. "<i>A Nondisturbing Service to Automatically Customize Notification Sending Using Implicit-Feedback.</i>" Scientific Programming 2019 (2019).</cite>

https://www.hindawi.com/journals/sp/2019/1293194/
