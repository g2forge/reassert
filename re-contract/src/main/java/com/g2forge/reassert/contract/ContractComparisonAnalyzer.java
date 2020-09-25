package com.g2forge.reassert.contract;

import java.util.LinkedHashSet;
import java.util.Set;
import java.util.stream.Collectors;

import org.slf4j.event.Level;

import com.g2forge.alexandria.annotations.note.Note;
import com.g2forge.alexandria.annotations.note.NoteType;
import com.g2forge.alexandria.java.core.helpers.HCollection;
import com.g2forge.alexandria.java.core.helpers.HCollector;
import com.g2forge.alexandria.java.fluent.optional.IOptional;
import com.g2forge.alexandria.java.fluent.optional.NullableOptional;
import com.g2forge.alexandria.java.type.function.TypeSwitch1.FunctionBuilder;
import com.g2forge.reassert.contract.eval.AnalyzeTermExpressionEvaluator;
import com.g2forge.reassert.contract.eval.TermRelationOperationSystem;
import com.g2forge.reassert.contract.model.finding.ExpressionContextFinding;
import com.g2forge.reassert.contract.model.finding.IFindingFactory;
import com.g2forge.reassert.contract.model.finding.UnrecognizedTermFinding;
import com.g2forge.reassert.contract.model.name.AContractComparisonName;
import com.g2forge.reassert.contract.model.name.BContractComparisonName;
import com.g2forge.reassert.contract.model.name.ContractComparisonName;
import com.g2forge.reassert.contract.model.name.IContractComparisonName;
import com.g2forge.reassert.contract.model.rule.IContractComparisonRule;
import com.g2forge.reassert.contract.model.rule.IContractComparisonRules;
import com.g2forge.reassert.core.model.contract.IContractApplied;
import com.g2forge.reassert.core.model.contract.IContractTerms;
import com.g2forge.reassert.core.model.contract.license.ILicenseTerm;
import com.g2forge.reassert.core.model.contract.terms.ITerm;
import com.g2forge.reassert.core.model.contract.terms.ITerms;
import com.g2forge.reassert.core.model.contract.terms.TermRelation;
import com.g2forge.reassert.core.model.report.IFinding;
import com.g2forge.reassert.core.model.report.IFindingConsumer;
import com.g2forge.reassert.express.eval.ExplainingEvaluator;
import com.g2forge.reassert.express.eval.IEvaluator;
import com.g2forge.reassert.express.eval.ReductionRewriter;
import com.g2forge.reassert.express.eval.ValueEvaluator;
import com.g2forge.reassert.express.eval.value.ObjectValueSystem;
import com.g2forge.reassert.express.model.IExplained;
import com.g2forge.reassert.express.model.IExpression;
import com.g2forge.reassert.express.model.constant.Literal;
import com.g2forge.reassert.express.model.environment.ATypeSwitchEnvironment;
import com.g2forge.reassert.express.model.variable.Closure;

import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.ToString;

@Getter
@RequiredArgsConstructor
public class ContractComparisonAnalyzer implements IContractComparisonAnalyzer {
	@Data
	@Builder(toBuilder = true)
	@RequiredArgsConstructor
	@EqualsAndHashCode(callSuper = false)
	@ToString(callSuper = false)
	public static class ContractEnvironment extends ATypeSwitchEnvironment<IContractComparisonName, TermRelation> {
		protected final IContractTerms a;

		protected final IContractTerms b;

		@Override
		protected void with(FunctionBuilder<IContractComparisonName, IOptional<? extends IExpression<IContractComparisonName, TermRelation>>> builder) {
			builder.add(ContractComparisonName.class, name -> {
				@SuppressWarnings("unchecked")
				final ITerms<ITerm> cast = (ITerms<ITerm>) name.getContract().getTerms();
				return of(cast.getRelation(name.getTerm()));
			});
			builder.add(AContractComparisonName.class, name -> {
				final ITerm term = name.getTerm();
				@SuppressWarnings("unchecked")
				final ITerms<ITerm> terms = (ITerms<ITerm>) getA().getTerms();
				final TermRelation relation = terms.getRelation(term);
				final ContractComparisonName literalName = new ContractComparisonName(name.getScheme(), term, getA());
				return NullableOptional.of(new Literal<>(literalName, relation));
			});
			builder.add(BContractComparisonName.class, name -> {
				final ITerm term = name.getTerm();
				@SuppressWarnings("unchecked")
				final ITerms<ITerm> terms = (ITerms<ITerm>) getB().getTerms();
				final TermRelation relation = terms.getRelation(term);
				final ContractComparisonName literalName = new ContractComparisonName(name.getScheme(), term, getB());
				return NullableOptional.of(new Literal<>(literalName, relation));
			});
		}
	}

	protected final IContractComparisonRules rules;

	@Note(type = NoteType.TODO, value = "Implement license operations", issue = "G2-919")
	@Override
	public void analyze(IContractApplied aApplied, IContractApplied bApplied, IFindingConsumer consumer) {
		final IContractTerms a = (IContractTerms) aApplied;
		final IContractTerms b = (IContractTerms) bApplied;
		final ContractEnvironment environment = new ContractEnvironment(a, b);

		// Sets of usage terms we need to approve and license conditions we need to meet
		final Set<ITerm> remainingTermsA = toRemainingTerms(a), remainingTermsB = toRemainingTerms(b);

		final IEvaluator<IContractComparisonName, TermRelation, IExplained<TermRelation>> evaluator = new ExplainingEvaluator<>(ObjectValueSystem.create(), TermRelationOperationSystem.create());
		final IEvaluator<IContractComparisonName, TermRelation, IExpression<IContractComparisonName, TermRelation>> reduce = new ReductionRewriter<>(new ValueEvaluator<>(ObjectValueSystem.create(), TermRelationOperationSystem.create()), ReductionRewriter.Reduction.ApplyClosures);
		for (IContractComparisonRule rule : getRules().getRules()) {
			final IExpression<IContractComparisonName, TermRelation> expression = rule.getExpression();
			final IFindingFactory<?> findingFactory = rule.getFinding();

			final ExpressionContextFinding analyzed;
			final IExplained<TermRelation> explained;

			if (expression != null) {
				// Compute the input and output terms
				final AnalyzeTermExpressionEvaluator analyzer = new AnalyzeTermExpressionEvaluator(termRelation -> {
					if (findingFactory == null) return TermRelation.Excluded.equals(termRelation);
					return findingFactory.apply(new Literal<>(termRelation)).getLevel().equals(Level.INFO);
				});
				analyzed = analyzer.eval(expression);
				if (!analyzed.getInputs().containsAll(analyzed.getOutputs())) throw new IllegalArgumentException(String.format("Rule to satisfy \"%1$s\" did not use \"%2$s\"", analyzed.getOutputs().stream().map(ITerm::getDescription).collect(HCollector.joining(", ", " & ")), HCollection.difference(analyzed.getOutputs(), analyzed.getInputs()).stream().map(ITerm::getDescription).collect(HCollector.joining(", ", " & "))));

				// Evaluate the expression
				final IExpression<IContractComparisonName, TermRelation> reduced = reduce.eval(new Closure<>(environment, expression));
				explained = evaluator.eval(reduced);

				// Record that we handled the output terms
				remainingTermsA.removeAll(analyzed.getOutputs());
				remainingTermsB.removeAll(analyzed.getOutputs());
			} else {
				analyzed = new ExpressionContextFinding(null, HCollection.emptySet(), HCollection.emptySet(), null);
				explained = new Literal<>(TermRelation.Included);
			}

			// Create & contextualize the finding
			if (findingFactory != null) {
				final IFinding finding = findingFactory.apply(explained);
				consumer.found(analyzed.toBuilder().inputs(HCollection.difference(analyzed.getInputs(), analyzed.getOutputs())).finding(finding).build());
			}
		}

		// Report an error if any of the terms in our input weren't recognized
		remainingTermsA.stream().map(UnrecognizedTermFinding::new).forEach(consumer::found);
		remainingTermsB.stream().map(UnrecognizedTermFinding::new).forEach(consumer::found);
	}

	protected LinkedHashSet<ITerm> toRemainingTerms(final IContractTerms contract) {
		return contract.getTerms().getTerms(true).stream().filter(term -> (!(term instanceof ILicenseTerm)) || ILicenseTerm.Type.Condition.equals(((ILicenseTerm) term).getType())).collect(Collectors.toCollection(LinkedHashSet::new));
	}
}