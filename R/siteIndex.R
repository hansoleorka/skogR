#' Site index
#' 
#' Compute site index curves
#' 
#' @param age age in breast height
#' @param HL Lorey's mean height
#' @param SP species code (1=spruce, 2= pine, 3= brich)
#' @param Vestlandet character for using functions representative for western-Norway either "coast" or "interior"
#' @return V Volume including bark (m3/ha)
#' @details ?
#' @author Hans Ole Ørka \email{hans.ole.orka@@gmail.org}
#' @export

siteindex <- function(age,HL,SP){

     # Calculate H40 SPRUCE
     if(SP == 1)
     H_G17 =((age+5.5)/(4.30606+0.164818*(age+5.5)))**2.1;
     a = (age-40);
     diff = 3+0.040183*a-0.104701*a**2/100+0.679104*a**3/100000+0.184402*a**4/1000000-0.224249*a**5/100000000;
     H_spruce = H_G17 + (si-17) * diff/3;
     SI <- ((H_spruce/(diff/3)) - H_G17) + 17 #inverter 
     
     # Calculate H40 PINE 
     H_F14 = 24.7*(1-exp(-0.02105*age))**1.18029+1.3;
     a = (age-40)/10;
     diff = 3 + 0.394624 * a - 0.0649695 * a**2 + 0.487394 * a**3/100 - 0.141827 * a**4/1000;
     H_pine = H_F14 + (si-14) * diff/3;
     SI <- ((H_pine/(diff/3)) - H_F14) + 14 #inverter

     # Calculate H40 BIRCH 
     # Fra Braastad (1977) (gir gjennomsnitshøyde fra overhøyde ved 40 års brysthøydealder (HL40)) 
     a1=2.336;
     a2=-0.0552;
     a3=-0.0326;
     a4=0.00260;
     a5=-0.0000574;
     a=40*a5;
     b=a2+1+a4*40;
     c=a1+40*a3-si;
     HL40=(-b+(b**2-4*a*(c))**0.5)/(2*a);
     
     # Fra STRAND (1967) (gir gjennomsnittshøyde fra brysthøydealder og HL40-bonitet)
     B0=(40/(HL40-1.3)-0.6956)/1.12;
     HL_birch=age/(B0*(1+0.0003*age)+0.01739*age)+1.3;
     # Fra Braastad (1977) (gir overhøyde fra gjennomsnittshøyde ved gitt alder)
     H_birch = HL_birch + (a1+a2*HL_birch+a3*age+a4*HL_birch*age+a5*HL_birch**2*age);



     #Calculate H40 SPRUCE (SHARMA & BRUNNER 2011)
     a0 = 40;
     b1=18.9206;
     b2=5175.18;
     b3=1.1576;
     h0 = si - 1.3;
     a1 = age;
     R=0.5*(h0-b1+((h0-b1)**2+4*b2*h0*a0**(-b3))**0.5);
     h1=(b1+R)/(1+(b2/R*a1**(-b3)));
     H_sp_new= h1 + 1.3;

     #Calculate H40 PINE (SHARMA & BRUNNER 2011)
     a0 = 40;
     b1=12.8361;
     b2=3263.99;
     b3=1.1758;
     h0 = si - 1.3;
     a1 = age;
     R=0.5*(h0-b1+((h0-b1)**2+4*b2*h0*a0**(-b3))**0.5);
     h1=(b1+R)/(1+(b2/R*a1**(-b3)));
     H_pi_new= h1 + 1.3;
}
