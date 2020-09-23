package com.g2forge.reassert.contract.algorithm.licenseusage.model.rule;

import com.g2forge.reassert.contract.algorithm.licenseusage.model.name.ILicenseUsageName;
import com.g2forge.reassert.contract.model.finding.IFindingFactory;
import com.g2forge.reassert.core.model.contract.terms.TermRelation;
import com.g2forge.reassert.express.model.IExpression;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class LicenseUsageRule implements ILicenseUsageRule {
	protected final IExpression<ILicenseUsageName, TermRelation> expression;

	protected final IFindingFactory<?> finding;
}
