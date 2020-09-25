package com.g2forge.reassert.contract.model.finding;

import com.g2forge.reassert.core.model.contract.terms.TermRelation;
import com.g2forge.reassert.core.model.report.IFinding;
import com.g2forge.reassert.express.model.IExplained;

public interface IContractTermFinding extends IFinding {
	public IExplained<TermRelation> getResult();
}
