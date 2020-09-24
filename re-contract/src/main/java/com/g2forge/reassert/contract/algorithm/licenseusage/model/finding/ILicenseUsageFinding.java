package com.g2forge.reassert.contract.algorithm.licenseusage.model.finding;

import com.g2forge.reassert.core.model.contract.terms.TermRelation;
import com.g2forge.reassert.core.model.report.ITerminalFinding;
import com.g2forge.reassert.express.model.IExplained;

public interface ILicenseUsageFinding extends ITerminalFinding {
	public IExplained<TermRelation> getResult();
}
