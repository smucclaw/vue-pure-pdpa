module Test.AnyAll.PdpaQ where

import AnyAll.Types
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Either (Either(..))

pdpaQ2 :: Q
pdpaQ2 = ( Q
    { andOr: And, children: [(Q
        { andOr: Or, children: [(Q
            { andOr: Or, children: [(Q
                { andOr: (Simply "26A.a.1 access"), children: [], mark: Unknown, prePost: Nothing, shouldView: Ask
                }),(Q
                { andOr: (Simply "26A.a.2 use"), children: [], mark: Unknown, prePost: Nothing, shouldView: Ask
                }),(Q
                { andOr: (Simply "26A.a.3 disclosure"), children: [], mark: Unknown, prePost: Nothing, shouldView: Ask
                }),(Q
                { andOr: (Simply "26A.a.4 copying"), children: [], mark: Unknown, prePost: Nothing, shouldView: Ask
                }),(Q
                { andOr: (Simply "spamming"), children: [], mark: Unknown, prePost: Nothing, shouldView: Ask
                }),(Q
                { andOr: (Simply "26A.a.5 modification"), children: [], mark: Unknown, prePost: Nothing, shouldView: Ask
                }),(Q
                { andOr: (Simply "26A.a.6 disposal"), children: [], mark: Unknown, prePost: Nothing, shouldView: Ask
            })], mark: Unknown, prePost: (Just (PrePost "any unauthorised" "of personal data")), shouldView: View
            }),(Q
            { andOr: Or, children: [(Q
                { andOr: (Simply "26A.b.1 access"), children: [], mark: Unknown, prePost: Nothing, shouldView: Ask
                }),(Q
                { andOr: (Simply "26A.b.2 use"), children: [], mark: Unknown, prePost: Nothing, shouldView: Ask
                }),(Q
                { andOr: (Simply "26A.b.3 disclosure"), children: [], mark: Unknown, prePost: Nothing, shouldView: Ask
                }),(Q
                { andOr: (Simply "26A.b.4 copying"), children: [], mark: Unknown, prePost: Nothing, shouldView: Ask
                }),(Q
                { andOr: (Simply "26A.b.5 modification"), children: [], mark: Unknown, prePost: Nothing, shouldView: Ask
                }),(Q
                { andOr: (Simply "26A.b.6 disposal"), children: [], mark: Unknown, prePost: Nothing, shouldView: Ask
            })], mark: Unknown, prePost: (Just (PrePost "loss of storage medium on which personal data is stored in circumstances where the unauthorised" "of the personal data is likely to occur")), shouldView: View
        })], mark: Unknown, prePost: (Just (Pre "any of:")), shouldView: View
        }),(Q
        { andOr: (Simply "the data breach occurred only within an organisation"), children: [], mark: Unknown, prePost: Nothing, shouldView: Ask
        }),(Q
        { andOr: Or, children: [(Q
            { andOr: And, children: [(Q
                { andOr: And, children: [(Q
                    { andOr: Or, children: [(Q
                        { andOr: (Simply "full name"), children: [], mark: Unknown, prePost: Nothing, shouldView: Ask
                        }),(Q
                        { andOr: (Simply "alias"), children: [], mark: Unknown, prePost: Nothing, shouldView: Ask
                        }),(Q
                        { andOr: (Simply "identification number"), children: [], mark: Unknown, prePost: Nothing, shouldView: Ask
                    })], mark: Unknown, prePost: (Just (Pre "the data breach relates to the individual's")), shouldView: View
                    }),(Q
                    { andOr: Or, children: [(Q
                        { andOr: (Simply "1 The amount of any wages, salary, fee, commission, bonus, gratuity, allowance or other remuneration paid or payable to the individual by any person, whether under a contract of service or a contract for services."), children: [], mark: Unknown, prePost: Nothing, shouldView: Ask
                        }),(Q
                        { andOr: (Simply "2 The income of the individual from the sale of any goods or property."), children: [], mark: Unknown, prePost: Nothing, shouldView: Ask
                        }),(Q
                        { andOr: (Simply "3 The number of any credit card, charge card or debit card issued to or in the name of the individual."), children: [], mark: Unknown, prePost: Nothing, shouldView: Ask
                        }),(Q
                        { andOr: (Simply "4 The number assigned to any account the individual has with any organisation that is a bank or finance company."), children: [], mark: Unknown, prePost: Nothing, shouldView: Ask
                        }),(Q
                        { andOr: Or, children: [(Q
                            { andOr: (Simply "5.a is or had been the subject of any investigation under the CYPA;"), children: [], mark: Unknown, prePost: Nothing, shouldView: Ask
                            }),(Q
                            { andOr: (Simply "5.b is or had been arrested, on or after 1 July 2020, for an offence committed under any written law;"), children: [], mark: Unknown, prePost: Nothing, shouldView: Ask
                            }),(Q
                            { andOr: (Simply "5.c is or had been taken into care or custody by the Director-General of Social Welfare, a protector, any officer generally or specially authorised in that behalf in writing by the Director-General or protector or a police officer under the CYPA;"), children: [], mark: Unknown, prePost: Nothing, shouldView: Ask
                            }),(Q
                            { andOr: (Simply "5.d is attending or had attended a family programme in relation to an application to be made under section 50 of the CYPA;"), children: [], mark: Unknown, prePost: Nothing, shouldView: Ask
                            }),(Q
                            { andOr: (Simply "5.e is or was the subject of an order made by a court under the CYPA; or"), children: [], mark: Unknown, prePost: Nothing, shouldView: Ask
                            }),(Q
                            { andOr: (Simply "5.f is or had been concerned in any proceedings in any court or on appeal from any court, whether the individual is the person against or in respect of whom the proceedings are taken or a witness in those proceedings."), children: [], mark: Unknown, prePost: Nothing, shouldView: Ask
                        })], mark: Unknown, prePost: (Just (Pre "5 Any information that identifies, or is likely to lead to the identification of, the individual as a child or young person who —")), shouldView: View
                        }),(Q
                        { andOr: Or, children: [(Q
                            { andOr: (Simply "6.a the individual who has been or is the subject of any investigation, examination, assessment or treatment under the VAA relating to whether the individual is a vulnerable adult experiencing or at risk of abuse, neglect or self-neglect;"), children: [], mark: Unknown, prePost: Nothing, shouldView: Ask
                            }),(Q
                            { andOr: (Simply "6.b the individual as a vulnerable adult who has been committed to a place of temporary care and protection or place of safety or to the care of a fit person under the VAA;"), children: [], mark: Unknown, prePost: Nothing, shouldView: Ask
                            }),(Q
                            { andOr: (Simply "6.c the individual as a vulnerable adult who is the subject of an order made by a court under the VAA;"), children: [], mark: Unknown, prePost: Nothing, shouldView: Ask
                            }),(Q
                            { andOr: (Simply "6.d a place of temporary care and protection or place of safety in which an individual or a vulnerable adult mentioned in sub-paragraph (a), (b) or (c) is committed, or the location of such a place of temporary care and protection or place of safety; or"), children: [], mark: Unknown, prePost: Nothing, shouldView: Ask
                            }),(Q
                            { andOr: (Simply "6.e a fit person under whose care an individual or a vulnerable adult mentioned in sub-paragraph (a), (b) or (c) is placed, or the location of the premises of such a fit person."), children: [], mark: Unknown, prePost: Nothing, shouldView: Ask
                        })], mark: Unknown, prePost: (Just (Pre "6 Any information that identifies, or is likely to lead to the identification of — b")), shouldView: View
                        }),(Q
                        { andOr: Or, children: [(Q
                            { andOr: (Simply "7.a to create a secure electronic record or secure electronic signature;"), children: [], mark: Unknown, prePost: Nothing, shouldView: Ask
                            }),(Q
                            { andOr: (Simply "7.b to verify the integrity of a secure electronic record; or"), children: [], mark: Unknown, prePost: Nothing, shouldView: Ask
                            }),(Q
                            { andOr: (Simply "7.c to verify the authenticity or integrity of a secure electronic signature."), children: [], mark: Unknown, prePost: Nothing, shouldView: Ask
                        })], mark: Unknown, prePost: (Just (Pre "7 Any private key of or relating to the individual that is used or may be used —")), shouldView: View
                        }),(Q
                        { andOr: (Simply "8 The net worth of the individual."), children: [], mark: Unknown, prePost: Nothing, shouldView: Ask
                        }),(Q
                        { andOr: (Simply "9 The deposit of moneys by the individual with any organisation."), children: [], mark: Unknown, prePost: Nothing, shouldView: Ask
                        }),(Q
                        { andOr: (Simply "10 The withdrawal by the individual of moneys deposited with any organisation."), children: [], mark: Unknown, prePost: Nothing, shouldView: Ask
                        }),(Q
                        { andOr: (Simply "11 The granting by an organisation of advances, loans and other facilities by which the individual, being a customer of the organisation, has access to funds or financial guarantees."), children: [], mark: Unknown, prePost: Nothing, shouldView: Ask
                        }),(Q
                        { andOr: (Simply "12 The incurring by the organisation of any liabilities other than those mentioned in paragraph 11 on behalf of the individual."), children: [], mark: Unknown, prePost: Nothing, shouldView: Ask
                        }),(Q
                        { andOr: (Simply "13 The payment of any moneys, or transfer of any property, by any person to the individual, including the amount of the moneys paid or the value of the property transferred, as the case may be."), children: [], mark: Unknown, prePost: Nothing, shouldView: Ask
                        }),(Q
                        { andOr: (Simply "14 The creditworthiness of the individual."), children: [], mark: Unknown, prePost: Nothing, shouldView: Ask
                        }),(Q
                        { andOr: (Simply "15 The individual’s investment in any capital markets products."), children: [], mark: Unknown, prePost: Nothing, shouldView: Ask
                        }),(Q
                        { andOr: Or, children: [(Q
                            { andOr: (Simply "16.a owed by the individual to an organisation; or"), children: [], mark: Unknown, prePost: Nothing, shouldView: Ask
                            }),(Q
                            { andOr: (Simply "16.b owed by an organisation to the individual."), children: [], mark: Unknown, prePost: Nothing, shouldView: Ask
                        })], mark: Unknown, prePost: (Just (Pre "16 The existence, and amount due or outstanding, of any debt —")), shouldView: View
                        }),(Q
                        { andOr: Or, children: [(Q
                            { andOr: (Simply "17.a the terms and conditions of any accident and health policy or life policy (called in this item the applicable policy) of which the individual is the policy owner or under which the individual is a beneficiary;"), children: [], mark: Unknown, prePost: Nothing, shouldView: Ask
                            }),(Q
                            { andOr: (Simply "17.b the premium payable by the policy owner under the applicable policy;"), children: [], mark: Unknown, prePost: Nothing, shouldView: Ask
                            }),(Q
                            { andOr: (Simply "17.c the benefits payable to any beneficiary under the applicable policy;"), children: [], mark: Unknown, prePost: Nothing, shouldView: Ask
                            }),(Q
                            { andOr: (Simply "17.d any information relating to any claim on, or payment under, the applicable policy, including the condition of the health of the individual and the diagnosis, treatment, prevention or alleviation of any ailment, condition, disability, disease, disorder or injury that the individual has suffered or is suffering from;"), children: [], mark: Unknown, prePost: Nothing, shouldView: Ask
                            }),(Q
                            { andOr: (Simply "17.e any other information that the individual is the policy owner of, or a beneficiary under, an applicable policy."), children: [], mark: Unknown, prePost: Nothing, shouldView: Ask
                        })], mark: Unknown, prePost: (Just (Pre "17 Any of the following:")), shouldView: View
                        }),(Q
                        { andOr: Or, children: [(Q
                            { andOr: (Simply "18.a any sexually-transmitted disease such as Chlamydial Genital Infection, Gonorrhoea and Syphilis;"), children: [], mark: Unknown, prePost: Nothing, shouldView: Ask
                            }),(Q
                            { andOr: (Simply "18.b Human Immunodeficiency Virus Infection;"), children: [], mark: Unknown, prePost: Nothing, shouldView: Ask
                            }),(Q
                            { andOr: (Simply "mental illness;"), children: [], mark: Unknown, prePost: Nothing, shouldView: Ask
                            }),(Q
                            { andOr: (Simply "18.c schizophrenia or delusional disorder;"), children: [], mark: Unknown, prePost: Nothing, shouldView: Ask
                            }),(Q
                            { andOr: (Simply "18.d substance abuse and addiction, including drug addiction and alcoholism"), children: [], mark: Unknown, prePost: Nothing, shouldView: Ask
                        })], mark: Unknown, prePost: (Just (Pre "18 The assessment, diagnosis, treatment, prevention or alleviation by a health professional of any of the following affecting the individual:")), shouldView: View
                        }),(Q
                        { andOr: Or, children: [(Q
                            { andOr: (Simply "19.a the donation or receipt of a human egg or human sperm; or"), children: [], mark: Unknown, prePost: Nothing, shouldView: Ask
                            }),(Q
                            { andOr: (Simply "19.b any contraceptive operation or procedure or abortion."), children: [], mark: Unknown, prePost: Nothing, shouldView: Ask
                        })], mark: Unknown, prePost: (Just (Pre "19 The provision of treatment to the individual for or in respect of —")), shouldView: View
                        }),(Q
                        { andOr: Or, children: [(Q
                            { andOr: (Simply "20.a subject to section 4(4)(b) of the Act, the donation and removal of any organ from the body of the deceased individual for the purpose of its transplantation into the body of another individual;"), children: [], mark: Unknown, prePost: Nothing, shouldView: Ask
                            }),(Q
                            { andOr: (Simply "20.b the donation and removal of any specified organ from the individual, being a living organ donor, for the purpose of its transplantation into the body of another individual;"), children: [], mark: Unknown, prePost: Nothing, shouldView: Ask
                            }),(Q
                            { andOr: (Simply "20.c the transplantation of any organ mentioned in sub-paragraph (a) or (b) into the body of the individual."), children: [], mark: Unknown, prePost: Nothing, shouldView: Ask
                        })], mark: Unknown, prePost: (Just (Pre "20 Any of the following:")), shouldView: View
                        }),(Q
                        { andOr: (Simply "21 Subject to section 4(4)(b) of the Act, the suicide or attempted suicide of the individual."), children: [], mark: Unknown, prePost: Nothing, shouldView: Ask
                        }),(Q
                        { andOr: (Simply "22 Domestic abuse, child abuse or sexual abuse involving or alleged to involve the individual."), children: [], mark: Unknown, prePost: Nothing, shouldView: Ask
                        }),(Q
                        { andOr: Or, children: [(Q
                            { andOr: (Simply "23.a information that the individual is or had been adopted pursuant to an adoption order made under the Adoption of Children Act (Cap. 4), or is or had been the subject of an application for an adoption order;"), children: [], mark: Unknown, prePost: Nothing, shouldView: Ask
                            }),(Q
                            { andOr: (Simply "23.b the identity of the natural father or mother of the individual;"), children: [], mark: Unknown, prePost: Nothing, shouldView: Ask
                            }),(Q
                            { andOr: (Simply "23.c the identity of the adoptive father or mother of the individual;"), children: [], mark: Unknown, prePost: Nothing, shouldView: Ask
                            }),(Q
                            { andOr: (Simply "23.d the identity of any applicant for an adoption order;"), children: [], mark: Unknown, prePost: Nothing, shouldView: Ask
                            }),(Q
                            { andOr: (Simply "23.e the identity of any person whose consent is necessary under that Act for an adoption order to be made, whether or not the court has dispensed with the consent of that person in accordance with that Act;"), children: [], mark: Unknown, prePost: Nothing, shouldView: Ask
                            }),(Q
                            { andOr: (Simply "23.f any other information that the individual is or had been an adopted child or relating to the adoption of the individual."), children: [], mark: Unknown, prePost: Nothing, shouldView: Ask
                        })], mark: Unknown, prePost: (Just (Pre "23 Any of the following:")), shouldView: View
                    })], mark: Unknown, prePost: (Just (Pre "any of:")), shouldView: View
                })], mark: Unknown, prePost: (Just (Pre "all of:")), shouldView: View
                }),(Q
                { andOr: Or, children: [(Q
                    { andOr: (Simply "the organisation has taken any action "), children: [], mark: Unknown, prePost: Nothing, shouldView: Ask
                    }),(Q
                    { andOr: (Simply "the organisation already implemented any technological measure to render it unlikely that the notifiable data breach will result in significant harm to the individual"), children: [], mark: Unknown, prePost: Nothing, shouldView: Ask
                })], mark: Unknown, prePost: (Just (Pre "any of:")), shouldView: View
            })], mark: Unknown, prePost: (Just (Pre "all of:")), shouldView: View
            }),(Q
            { andOr: (Simply "the number of affected individuals relLT the prescribed threshold of affected individuals"), children: [], mark: Unknown, prePost: Nothing, shouldView: Ask
            })], mark: Unknown, prePost: (Just (Pre "any of:")), shouldView: View
        })], mark: Unknown, prePost: (Just (Pre "all of:")), shouldView: View
    })