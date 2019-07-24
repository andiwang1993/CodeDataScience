% S¨¦parateurs ¨¤ Vaste Marge
% Cas lin¨¦aire, non lin¨¦aire, marges "souples"
% structure du code: c'est juste une proposition, ¨¤ vous de voir

clear all
close all


 load 'data6' X lab     % chargement des donn¨¦es 
%X(1,1)=1;
%X(1,2)=2;
%X(2,1)=1;
%X(2,2)=2;
%lab(1,1)=-1;
%lab(2,1)=1;
  
% bornes pour le dessin 2D
xmin=min(X(1,:));
ymin=min(X(2,:));
xmax=max(X(1,:));
ymax=max(X(2,:));

na=length(X); % nombre de points (d'apprentissage)

% dessin des points dans R^2

for i=1:na
    if lab(i)==1
        plot(X(1,i),X(2,i),'o','linewidth',2)
        hold on
    else
        plot(X(1,i),X(2,i),'x','linewidth',2,'markersize',12)
        hold on
    end
end
axis([xmin xmax ymin ymax])
grid
axis equal
hold on

pause 

% assemblage matrice de la forme quadratique du probl¨¨me dual 
A=zeros(na); %initialisation   
% A vous de construire A


% gradient ?pas constant pour probl¨¨me dual
alph0=0.5*ones(na,1); % point de départ (0, ou autre): réglable
pasgrad=5e-3;         % pas du gradient : parametre réglable
u=ones(na,1);         % vecteur de 1
crit_arret=1;         % initialisation critere d'arret
npas=0;               % comptage du nombre de pas
npas_max=100000;      % garde-fou convergence (si ça ne converge pas...)
epsi=1e-5;            % seuil convergence
sig=2;
%Conception de la matrice M
A=ones(na,1);
M=A*A'; %On conçoit ici la matrice de dim (na*na, remplissage par des 1), que l'on va maintenant remplir 
for i=1:na
    for j=1:na
        M(i,j)=lab(i)*lab(j)*exp(-(norm(X(:,i)-X(:,j))^2)/(sig^2));
    end
end
%On a bien H(alpha) = -1/2*alphaT.*M.*alpha - AT.*alpha
    
while and(crit_arret>epsi,npas<npas_max)
    npas=npas+1;
    
    
%     H(npas)=sum(alph0)-1/2*sum(sum(M));
    gradH=-(M*alph0-u); %Calcul du gradient
    alph=alph0+pasgrad*gradH;
    alph=alph-alph'*lab/(lab'*lab)*lab;
    alph = max(alph, zeros(na,1)); %Projection sur R4+
    
    %Test de convergence
    Conv=norm(alph-alph0);
    if Conv<epsi
        crit_arret = 0;
    end
    alph0=alph;
    
    % ici la boucle de gradient ¨¤ pas constant
    
    % ne pas oublier les projections....
    if Conv<epsi
        crit_arret=epsi/2 ; % je le mets juste pour ¨¦viter la boucle infinie
    end                 % tant que le code n'est pas ¨¦crit
end
%w=zeros(2,1);
%for i = 1:na
%    w=w+ alph0(i)*lab(i)*X(:,i);
%end

% Ce n'est pas fini! Il faut construire la fonction de decision <w,x> + b
% tracer ses isovaleurs 0 (la droite de partage) et +1 et -1 : les marges
% donc calculer b d'abord, et <w,x> pour tout x 

% comment calculer b? id¨¦e: en se servant des "points supports" x_s pour
% lesquels <w,x_s> +b = + ou - 1
% comment trouver les points support? Qu'est ce qui le caract¨¦rise?? 

Points_support=find(alph>epsi)'; % points supports = contraintes actives...alph>0
                                % aux erreurs num¨¦riques près
%Si le point constitue une contrainte active, alors on a b = 1 - lk*w'*xk
b=0;

c=1;
for i=Points_support
    
    s=0;
    for h=1:na
        s=s+alph0(h)*lab(h)*exp((-norm(X(:,h)-X(:,i))^2)/(sig^2));
    end
    btab(c)=lab(i)-s;
    c=c+1;
        
    
end
b=mean(btab);

    

% Calcul de b (on le calcule pour chaque pt support, puis on moyenne)
% on devrait v¨¦rifier que chaque pt support fournit le même b (aux erreurs
% num¨¦iques près, c'est pouquoi on moyenne)
nb_sup=0;
%for i=Points_support  % i d¨¦crit le tableau des indices des points supports
   %calcul de b pour chaque point support
%end


%calcul de w et trac?de la droite et de ses marges

% une m¨¦thode, qui parait lourde mais qui est la bonne dans le cas de 
% s¨¦paration non lin¨¦aire, est de construire la fonction de d¨¦cision
% sur une grille et d'en tracer les trois isovaleurs avec une fonction
% Matlab

% fonction de d¨¦cision <w,x> + b 


xp=xmin:0.2:xmax;   % cr¨¦ation d'une grille 
yp=ymin:0.2:ymax;
npx=length(xp);
npy=length(yp);
for i=1:npx
    for j=1:npy
        % calcul de <w,x> + b sur une grille
        x=[xp(i);yp(j)];
        a=0;
        for k=1:na
            a=a+lab(k)*alph0(k)*exp((-norm(X(:,k)-x)^2)/(sig^2));
            %b= b -alph0(j)*exp((-norm(X(:,j)-X(:,i))^2)/(sig^2));
        end
        V(i,j)=a+b;
    end
end
hold on
contour(xp,yp,V',[-1 0 1],'linewidth',2,'color','r')% dessin des contours
axis([xmin xmax ymin ymax])
title(' Suport Vector Machine')
grid



