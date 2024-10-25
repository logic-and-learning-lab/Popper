%% download from https://starling.utdallas.edu/datasets/ddi-relational/
%% used in the paper
%% Dhami DS, Kunapuli G, Das M, Page D, Natarajan S. Drug-Drug Interaction Discovery: Kernel Learning from Heterogeneous Similarities. Smart Health (Amst). 2018;9-10:88-100. doi:10.1016/j.smhl.2018.07.007


body_pred(enzyme,2).
body_pred(target,2).
body_pred(transporter,2).
body_pred(enzymesubstrate,2).
body_pred(enzymeinhibitor,2).
body_pred(enzymeinducer,2).
%body_pred(targetsubstrate,2).
body_pred(targetantagonist,2).
body_pred(targetinducer,2).
body_pred(targetinhibitor,2).
body_pred(targetagonist,2).
body_pred(transportersubstrate,2).
body_pred(transporterinhibitor,2).
body_pred(transporterinducer,2).
head_pred(interacts,2).


type(enzyme,(enzyme,drug)).
type(target,(target,drug)).
type(transporter,(transporter,drug)).
type(enzymesubstrate,(drug,enzyme)).
type(enzymeinhibitor,(drug,enzyme)).
type(enzymeinducer,(drug,enzyme)).
type(targetsubstrate,(drug,target)).
type(targetantagonist,(drug,target)).
type(targetinducer,(drug,target)).
type(targetinhibitor,(drug,target)).
type(targetagonist,(drug,target)).
type(transportersubstrate,(drug,transporter)).
type(transporterinhibitor,(drug,transporter)).
type(transporterinducer,(drug,transporter)).
type(interacts,(drug,drug)).


direction(enzyme,(out,in)).
direction(target,(out,in)).
direction(transporter,(out,in)).
direction(enzymesubstrate,(in,out)).
direction(enzymeinhibitor,(in,out)).
direction(enzymeinducer,(in,out)).
direction(targetsubstrate,(in,out)).
direction(targetantagonist,(in,out)).
direction(targetinducer,(in,out)).
direction(targetinhibitor,(in,out)).
direction(targetagonist,(in,out)).
direction(transportersubstrate,(in,out)).
direction(transporterinhibitor,(in,out)).
direction(transporterinducer,(in,out)).
direction(interacts,(in,in)).
