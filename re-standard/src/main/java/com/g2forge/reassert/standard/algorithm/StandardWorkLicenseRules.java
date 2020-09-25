package com.g2forge.reassert.standard.algorithm;

import static com.g2forge.reassert.express.model.operation.BooleanOperation.or;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import com.g2forge.alexandria.java.core.enums.EnumException;
import com.g2forge.alexandria.java.core.helpers.HCollection;
import com.g2forge.alexandria.java.core.marker.ISingleton;
import com.g2forge.alexandria.java.function.IFunction1;
import com.g2forge.alexandria.java.type.function.TypeSwitch1;
import com.g2forge.reassert.contract.algorithm.worklicense.model.finding.IncompatibleWorkLicenseFinding;
import com.g2forge.reassert.contract.algorithm.worklicense.model.rule.AWorkLicenseRules;
import com.g2forge.reassert.contract.algorithm.worklicense.model.rule.AWorkLicenseRulesFactory;
import com.g2forge.reassert.contract.algorithm.worklicense.model.rule.AWorkLicenseRulesFactoryFactory;
import com.g2forge.reassert.contract.algorithm.worklicense.model.rule.IWorkLicenseRules;
import com.g2forge.reassert.contract.algorithm.worklicense.model.rule.IWorkLicenseRulesFactory;
import com.g2forge.reassert.contract.model.rule.IContractComparisonRule;
import com.g2forge.reassert.core.api.ReassertLegalOpinion;
import com.g2forge.reassert.core.model.Copy;
import com.g2forge.reassert.core.model.IEdge;
import com.g2forge.reassert.core.model.artifact.Depends;
import com.g2forge.reassert.core.model.artifact.Inherits;
import com.g2forge.reassert.core.model.artifact.Invokes;
import com.g2forge.reassert.core.model.contract.license.ILicenseApplied;
import com.g2forge.reassert.core.model.contract.license.ILicenseFamily;
import com.g2forge.reassert.core.model.contract.license.ILicenseSpecific;
import com.g2forge.reassert.core.model.contract.license.ILicenseTerm;
import com.g2forge.reassert.standard.model.contract.license.StandardLicense;
import com.g2forge.reassert.standard.model.contract.license.StandardLicenseFamily;
import com.g2forge.reassert.standard.model.contract.license.StandardLicenseTerm;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

public class StandardWorkLicenseRules extends AWorkLicenseRulesFactoryFactory implements ISingleton {
	@Getter
	@RequiredArgsConstructor
	public static class GPLWorkLicenseRulesFactory extends AWorkLicenseRulesFactory {
		@Getter
		@RequiredArgsConstructor
		public class Rules extends AWorkLicenseRules {
			protected final ILicenseFamily combinedLicense;

			@Getter(lazy = true)
			private final Set<ILicenseTerm> unknown = computeUnknown();

			@Override
			protected List<IContractComparisonRule> computeRules() {
				final List<IContractComparisonRule> rules = new ArrayList<>();

				for (ILicenseTerm term : getWorkLicense().getTerms().getTerms(true)) {
					if (ignored.contains(term)) continue;
					switch (term.getType()) {
						case Permission:
							rules.add(rule(b -> b.expression(or(b.notA(term), b.b(term))).finding(IncompatibleWorkLicenseFinding::new)));
							break;
						case Condition:
						case Limitation:
							rules.add(rule(b -> b.expression(or(b.a(term), b.notB(term))).finding(IncompatibleWorkLicenseFinding::new)));
							break;
						default:
							throw new EnumException(ILicenseTerm.Type.class, term.getType());
					}
				}

				return rules;
			}

			protected Set<ILicenseTerm> computeUnknown() {
				final Set<ILicenseTerm> workLicenseTems = getWorkLicense().getTerms().getTerms(true);
				final Set<ILicenseTerm> combinedLicenseTerms = getCombinedLicense().getTerms().getTerms(true);
				if (workLicenseTems.equals(combinedLicenseTerms)) return HCollection.emptySet();

				final Set<ILicenseTerm> union = HCollection.union(workLicenseTems, combinedLicenseTerms);
				final Set<ILicenseTerm> intersection = HCollection.intersection(workLicenseTems, combinedLicenseTerms);
				return HCollection.difference(union, intersection);
			}
		}

		protected static final Set<StandardLicenseTerm> ignored = HCollection.asSet(StandardLicenseTerm.PatentGrant, StandardLicenseTerm.NoRedistribution);

		protected final ILicenseFamily workLicense;

		@Override
		public IWorkLicenseRules apply(ILicenseFamily combinedLicense) {
			if (getWorkLicense().equals(combinedLicense)) return null;
			return new Rules(combinedLicense);
		}

		@Override
		protected void expand(final TypeSwitch1.FunctionBuilder<IEdge, Boolean> builder) {
			builder.add(Depends.class, e -> true);
			builder.add(Copy.class, e -> true);
			builder.add(Inherits.class, e -> true);
			builder.add(Invokes.class, e -> true);
		}
	}

	protected static final StandardWorkLicenseRules INSTANCE = new StandardWorkLicenseRules();

	public static StandardWorkLicenseRules create() {
		return INSTANCE;
	}

	protected StandardWorkLicenseRules() {}

	@ReassertLegalOpinion
	protected IFunction1<ILicenseApplied, IFunction1<ILicenseFamily, IWorkLicenseRulesFactory>> computeFunction() {
		final TypeSwitch1.FunctionBuilder<ILicenseFamily, IFunction1<ILicenseFamily, IWorkLicenseRulesFactory>> familyBuilder = new TypeSwitch1.FunctionBuilder<>();
		familyBuilder.add(StandardLicenseFamily.class, l -> {
			switch (l) {
				case BSD:
				case Apache:
					return null;
				case GPL:
					return GPLWorkLicenseRulesFactory::new;
				default:
					throw new EnumException(StandardLicenseFamily.class, l);
			}
		});
		final IFunction1<ILicenseFamily, IFunction1<ILicenseFamily, IWorkLicenseRulesFactory>> familyFunction = familyBuilder.build();

		final TypeSwitch1.FunctionBuilder<ILicenseApplied, IFunction1<ILicenseFamily, IWorkLicenseRulesFactory>> licenseBuilder = new TypeSwitch1.FunctionBuilder<>();
		licenseBuilder.add(StandardLicense.class, l -> {
			switch (l) {
				case Owner:
					return null;
				default:
					final ILicenseFamily family = l.getFamily();
					if (family == null) throw new EnumException(StandardLicense.class, l);
					return familyFunction.apply(family);
			}
		});
		licenseBuilder.add(ILicenseSpecific.class, l -> {
			final ILicenseFamily family = l.getFamily();
			if (family == null) throw new IllegalArgumentException(l.toString());
			return familyFunction.apply(family);
		});
		licenseBuilder.add(ILicenseFamily.class, familyFunction);
		return licenseBuilder.build();
	}
}
