package com.g2forge.reassert.contract.model;

import com.g2forge.alexandria.java.function.IConsumer1;
import com.g2forge.alexandria.java.function.builder.IBuilder;
import com.g2forge.reassert.contract.model.finding.IFindingFactory;
import com.g2forge.reassert.contract.model.name.AContractComparisonName;
import com.g2forge.reassert.contract.model.name.BContractComparisonName;
import com.g2forge.reassert.contract.model.name.IContractComparisonName;
import com.g2forge.reassert.contract.model.rule.ContractComparisonRule;
import com.g2forge.reassert.contract.model.rule.IContractComparisonRule;
import com.g2forge.reassert.core.model.contract.terms.ITerm;
import com.g2forge.reassert.core.model.contract.terms.TermRelation;
import com.g2forge.reassert.express.model.IExpression;
import com.g2forge.reassert.express.model.operation.BooleanOperation;
import com.g2forge.reassert.express.model.variable.Variable;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

public interface IContractComparisonScheme<A extends ITerm, B extends ITerm> {
	@RequiredArgsConstructor
	@Getter
	public static class RuleBuilder<A extends ITerm, B extends ITerm> implements IBuilder<ContractComparisonRule> {
		protected final IContractComparisonScheme<A, B> scheme;

		protected final ContractComparisonRule.ContractComparisonRuleBuilder builder = ContractComparisonRule.builder();

		public IExpression<IContractComparisonName, TermRelation> a(final A term) {
			return new Variable<>(new AContractComparisonName<>(getScheme(), term));
		}

		public IExpression<IContractComparisonName, TermRelation> b(final B term) {
			return new Variable<>(new BContractComparisonName<>(getScheme(), term));
		}

		@Override
		public ContractComparisonRule build() {
			return builder.build();
		}

		public RuleBuilder<A, B> expression(IExpression<IContractComparisonName, TermRelation> expression) {
			builder.expression(expression);
			return this;
		}

		public RuleBuilder<A, B> finding(IFindingFactory<?> finding) {
			builder.finding(finding);
			return this;
		}

		public IExpression<IContractComparisonName, TermRelation> notA(A term) {
			return BooleanOperation.not(a(term));
		}

		public IExpression<IContractComparisonName, TermRelation> notB(B term) {
			return BooleanOperation.not(b(term));
		}
	}

	public default IContractComparisonRule rule(final IConsumer1<? super IContractComparisonScheme.RuleBuilder<A, B>> consumer) {
		final RuleBuilder<A, B> builder = new RuleBuilder<>(this);
		consumer.accept(builder);
		return builder.build();
	}

	public String getAName();

	public String getBName();
}
