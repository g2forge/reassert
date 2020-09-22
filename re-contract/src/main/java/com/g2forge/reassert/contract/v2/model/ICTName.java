package com.g2forge.reassert.contract.v2.model;

import java.util.stream.Stream;

import com.g2forge.alexandria.java.core.helpers.HStream;
import com.g2forge.alexandria.java.type.ref.ITypeRef;
import com.g2forge.reassert.core.model.contract.license.ILicenseTerm;
import com.g2forge.reassert.core.model.contract.terms.ITerm;
import com.g2forge.reassert.core.model.contract.usage.IUsageTerm;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

public interface ICTName {
	@Getter
	@RequiredArgsConstructor
	public enum ContractType {
		License(ITypeRef.of(ILicenseTerm.class)),
		Usage(ITypeRef.of(IUsageTerm.class));

		public static ContractType valueOf(ITerm term) {
			return HStream.findOne(Stream.of(ContractType.values()).filter(type -> type.getTermType().isInstance(term)));
		}

		protected final ITypeRef<? extends ITerm> termType;
	}

	public ContractType getContractType();

	public ITerm getTerm();
}
