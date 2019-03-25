# R-Ladies for PAWS Datathon

## Description

The 2019 R-Ladies Philly Community Data Project aims to help the [Philadelphia Animal Welfare Society (PAWS)](https://phillypaws.org/) improve its adoptions processes. PAWS is a non-profit organization dedicated to saving Philadelphia’s homeless and at-risk animals. PAWS is the city’s largest rescue partner and provider of low-cost, basic veterinary care for pet owners and rescue organizations that cannot otherwise access or afford it. Through its 3 no-kill shelters, foster care network, and special events, PAWS finds loving homes for thousands of animals each year. PAWS is working to make Philadelphia a no-kill city where every healthy and treatable pet is guaranteed a home.

For this data challenge, PAWS has made 2018 data available containing adoption application form submissions, staff processing of applications, and animal outcome data. We will develop analytic approaches to address the following topics: 

### 1. Describe an animal's trajectory at PAWS

* 1.1 What is the time from intake to outcome, based on animal type, PAWS intake location, and PAWS outcome location? Additional descriptives welcome!  
* 1.2 What are animal-specific factors that speed up the adoption process? As above, differentiate between animal species and PAWS location.  
* 1.3 Are there seasonal patterns to intakes and outcomes? As above, differentiate between animal species and PAWS location.  
* 1.4 Free-form data mining within the PetPoint dataset. You can look at subtypes of animals, whether the intake was a stray, returned, or transferred in, healthy vs. treatable status, etc.  

### 2. Describe an adoption application's trajectory at PAWS

* 2.1 How long does each step of an application take? (by animal species and PAWS location where available); i.e., from date of processing start, through checking off each checklist item, to the date and time when an animal was adopted.
* 2.2 What are the applicant and animal characteristics that predict a speedy adoption?
* 2.3 What are the applicant and animal characteristics that predict a denied application?  
* 2.4 Are there seasonal patterns to adoptions?  
* 2.5 What predicts an applicant who adopts elsewhere?  

### 3. GIS data mining

A few ideas:
* 3.1 Are the people who want a dog for protection in areas that have higher crime?
* 3.2 Are there geographic clusters of areas where pets are returned?  Are these areas in which the rental stock is comparatively high, compared to other areas in the metro area?
* 3.3 What is the socioeconomic mileu of successful adopters?  Are people in poorer / richer / more unemployment / less unemployment / etc. more likely to be successful adopters?
* 3.4 Are people from minority - rich tracts more likely to be red-flagged?
* 3.5 Is there a high concentration of applications from areas with a high latino population?  Should PAWS invest in Spanish language translations of their materials?
* 3.6 Do people in certain areas like certain breeds, sizes, colors of pets more? 
* how far away are our dog adopters from public parks?  Does that affect success of adoption?

### 4. Other data mining

Come up with your own questions and insights using the provided dataset.

## Timeline and Workflow

**February 12, 6-8pm: Kickoff Meetup:** At this event, teams will form to work on one of the 4 topics outlined above (any participant can choose to be a part of multiple groups). Interested participants should add their names, slack IDs and GitHub IDs to [this list](https://docs.google.com/document/d/1EzzVXFIJ-_KdAwprpDXTcC0G8VYxp6nCOKoZnskgEII/edit#heading=h.46g28hd7gts9) (new rows can be added). All participants should join the R-Ladies Philly Slack workspace with [this sign-up link](http://bit.ly/join-rladies-slack) and join the #paws channel. If anyone needs help setting up a GitHub account, there will be an organizer to help with this at the event. *If you weren't able to attend the Kickoff Meetup and want to get involved, check out [How to get involved](get_involved.md).*

After the kickoff meetup, groups will work together using Slack and GitHub. R-Ladies organizers will check in with groups via Slack to help with progress and help where necessary. 

**March 17 (week before conclusion meetup):** Submissions should be complete and presentations should be drafted for the conclusion meetup.

**March 26, 6-8pm: Conclusion Meetup:** This will be the conclusion meetup event, where groups will present their results and discuss their experiences. Each team will have the freedom to choose a participant to present the team's results, or to request that an R-Ladies Philly organizer presents the team's results. The latter option requires some advance coordination.

## Submission Format

Final submissions should be made in the form of a report, using the "PAWS_SubmissionForm" template. An example from a smaller scale 2018 analysis is available in the PAWS_SubmissionExample.pdf file. All analyses should be merged into the main GitHub repo via pull requests once analyses and reports are complete. Presentations of results should use [this template](https://docs.google.com/presentation/d/1KoOCCyjNBQTMlq19BpnA37o66BtXGdvpTz1N2ZU52hg/edit#slide=id.g35f391192_00).

## Data

The data contains closed applications from 2018 (the dates stretch into December 2017 and January 2019 in order to capture some applications that may not have a clean cutoff in 2018). The datasets are found in the /Data folder. A codebook is available describing each dataset, in the cleaned_data_codebook.md file.

## Code of Conduct

R-Ladies is dedicated to providing a harassment-free experience for everyone. We do not tolerate harassment of participants in any form. Please refer to our [code of conduct](https://github.com/rladies/starter-kit/wiki/Code-of-Conduct) for more information. Please do not hesitate to contact R-Ladies Philly organizers at philly@rladies.org or via Slack if you have any questions or concerns. 
