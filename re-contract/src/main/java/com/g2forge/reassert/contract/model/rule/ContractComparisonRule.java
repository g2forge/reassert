package com.g2forge.reassert.contract.model.rule;

import com.g2forge.reassert.contract.model.finding.IFindingFactory;
import com.g2forge.reassert.contract.model.name.IContractComparisonName;
import com.g2forge.reassert.core.model.contract.terms.TermRelation;
import com.g2forge.reassert.express.model.IExpression;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class ContractComparisonRule implements IContractComparisonRule {
	public static class ContractComparisonRuleBuilder implements IContractComparisonRule.IContractComparisonRuleBuilder<ContractComparisonRuleBuilder, ContractComparisonRule> {}

	protected final IExpression<IContractComparisonName, TermRelation> expression;

	protected final IFindingFactory<?> finding;
}
