package com.g2forge.reassert.contract.model.rule;

import com.g2forge.alexandria.java.function.builder.IBuilder;
import com.g2forge.reassert.contract.model.finding.IFindingFactory;
import com.g2forge.reassert.contract.model.name.AContractComparisonName;
import com.g2forge.reassert.contract.model.name.BContractComparisonName;
import com.g2forge.reassert.contract.model.name.IContractComparisonName;
import com.g2forge.reassert.core.model.contract.terms.ITerm;
import com.g2forge.reassert.core.model.contract.terms.TermRelation;
import com.g2forge.reassert.express.model.IExpression;
import com.g2forge.reassert.express.model.operation.BooleanOperation;
import com.g2forge.reassert.express.model.variable.Variable;

public interface IContractComparisonRule {
	public interface IContractComparisonRuleBuilder<A extends ITerm, B extends ITerm, Builder extends IContractComparisonRuleBuilder<A, B, Builder, Built>, Built extends IContractComparisonRule> extends IBuilder<Built> {
		public default IExpression<IContractComparisonName, TermRelation> a(final A term) {
			return new Variable<>(new AContractComparisonName(term));
		}

		public default IExpression<IContractComparisonName, TermRelation> b(final B term) {
			return new Variable<>(new BContractComparisonName(term));
		}

		public Builder expression(IExpression<IContractComparisonName, TermRelation> expression);

		public Builder finding(IFindingFactory<?> finding);

		public default IExpression<IContractComparisonName, TermRelation> notA(A term) {
			return BooleanOperation.not(a(term));
		}

		public default IExpression<IContractComparisonName, TermRelation> notB(B term) {
			return BooleanOperation.not(b(term));
		}
	}

	public IExpression<IContractComparisonName, TermRelation> getExpression();

	public IFindingFactory<?> getFinding();
}
